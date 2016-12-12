"""
Develop with testing:

...
"""
module Plans


"Sample of address object type"
immutable Sample
    addr::AbstractString
end
export Sample


"Canstructs Sample object"
sample( filename::AbstractString) = Sample( filename)
export sample


"""Card construct objects described regex::Regex functions: need(), prepare(), iter()"""
immutable Card
    sample::Sample
    regex::Regex
    need::Function
    prepare::Function
    ready::Function    
    readable::Function
    writable_tmp::Function
    iter::Function
end
export Card


card(
    sample::Sample, 
    regex::Regex; 
    need::Function=(p)->nothing, 
    prepare::Function=(p)->nothing, 
    ready::Function= (p)-> ( str=string(p); ismatch( r"\.gz(?=ip)$"i, str) ? filesize( str)>20 : filesize( str)>0 ), 
    readable::Function=(p)-> ( str=string(p); ismatch( r"\.gz(?=ip)$"i, str) ? `zcat $str` : str ),
    writable_tmp::Function=(p)-> ( str=string(p); ismatch( r"\.gz(?=ip)$"i, str) ? `gzip $str.TMP` : str*".TMP" ),
    iter::Function=(p)-> eachline( readable( p))
)::Card = 
    Card( sample, regex, need, prepare, ready, readable, writable_tmp, iter )
export card


"Sample(\"lala\") |> card( regex, ...)"
card( regex::Regex )::Function = 
    (s::Sample)-> card( s, regex )


"""
    Card(...) |> ready_will() do plan
        
     filesize(\"\$plan\")>0
     
    end # --> new Card (with 'need' field)
"""
function ready_will(f::Function)::Function 
    plan2 = "" # хотелось бы чтоб и тип был
    try method = @which( f(plan2)) # будет ошибка если нет метода на аргументах
    catch e
        error("Bad function signature returned by ready_will() $e")
    end    

    (c::Card)->
        card( c.sample, c.regex, 
            need=c.need, prepare=c.prepare, ready=f, readable=c.readable, writable_tmp=c.writable_tmp, iter=c.iter)
end            
export ready_will


"""
    Card(...) |> need_will() do plan 
    
       ... use plan.addr::RegexMatch here 
      
      for create and return vector string
    
    end # --> new Card (with 'need' field)
"""
function need_will(f::Function)::Function
    plan2 = "" # хотелось бы чтоб и тип был
    try method = @which( f(plan2)) # будет ошибка если нет метода на аргументах
    catch e
        error("Bad function signature returned by need_will() $e")
    end    

    (c::Card)->
        card( c.sample, c.regex, need=f, prepare=c.prepare, ready=c.ready, readable=c.readable, writable_tmp=c.writable_tmp, iter=c.iter)
end
export need_will


"""
    Card(...) |> 
    
    prepare_will() do plan,needs 
    
     .... use \"\$plan\" , needs
    
    end # --> new Card (with 'need' field)
    
"""
function prepare_will(f::Function)::Function  
    plan2 = "" # тут хорошо бы получить значения нужных типов
    needs2 = ""
    try method = @which( f(plan2,needs2)) # будет ошибка если нет метода на аргументах
    catch e
        error("Bad function signature returned by prepare_will() $e")
    end    

    (c::Card)->
        card( c.sample, c.regex, need=c.need, prepare=f, ready=c.ready, readable=c.readable, writable_tmp=c.writable_tmp, iter=c.iter)
end        
export prepare_will


"""
    Card(...) |> readable_will() do plan
    
     .... must return something readable ... 
    
    end # --> new Card 
"""
function readable_will(f::Function)::Function 
    plan2 = "" # хотелось бы чтоб и тип был
    try method = @which( f(plan2)) # будет ошибка если нет метода на аргументах
    catch e
        error("Bad function signature returned by readable_will() $e")
    end    

    (c::Card)->
        card( c.sample, c.regex, 
            need=c.need, prepare=c.prepare, ready=c.ready, readable=f, writable_tmp=c.writable_tmp, iter=c.iter)
end
export readable_will



"""
    Card(...) |> writable_tmp_will() do plan
    
     .... must return something writable for write TMP ... 
    
    end # --> new Card 
"""
function writable_tmp_will(f::Function)::Function 
    plan2 = "" # хотелось бы чтоб и тип был
    try method = @which( f(plan2)) # будет ошибка если нет метода на аргументах
    catch e
        error("Bad function signature returned by writable_tmp_will() $e")
    end    

    (c::Card)->
        card( c.sample, c.regex, 
            need=c.need, prepare=c.prepare, ready=c.ready, readable=c.readable, writable_tmp=f, iter=c.iter)
end
export writable_tmp_will



"""
    Card(...) |> iter_will() do plan 
        
        .... must return something iterable ... 
        
    end # --> new Card (with 'need' field)
"""
function iter_will(f::Function)::Function
    plan2 = "" # хотелось бы чтоб и тип был
    try method = @which( f(plan2)) # будет ошибка если нет метода на аргументах
    catch e
        error("Bad function signature returned by iter_will() $e")
    end    

    (c::Card)->
        card( c.sample, c.regex, 
            need=c.need, prepare=c.prepare, ready=c.ready, readable=c.readable, writable_tmp=c.writable_tmp, iter=f)
end
export iter_will


abstract Plan


"Plan describes matched address, typeof(source), typeof(codec), need() function"
immutable Solved<:Plan
    addr::RegexMatch
    need::Function
    prepare::Function
    ready::Function
    readable::Function
    writable_tmp::Function
    iter::Function
end



"Used for unmatched addresses"
immutable Trouble<:Plan
    addr::AbstractString
end


export Plan, Solved, Trouble


import Base.string
"Returns address of plan as string"
string(p::Solved) = p.addr.match
string(t::Trouble) = t.addr
export string


"(Array{cards}, address) -> plan"
function plan{C<:Card}( cc::Array{C}, adr::AbstractString )::Plan
    for c in cc
        w = plan( c, adr)
        typeof(w) <: Solved && return w
    end
    _trouble(adr)
end
export plan


"(card, address)->plan"
function plan( c::Card, adr::AbstractString)::Plan
    if (m=match(c.regex, adr ))!=nothing
        Solved( m, c.need, c.prepare, c.ready, c.readable, c.writable_tmp, c.iter)
    else
        Trouble(adr)
    end
end



"""Recursive find all plans depended from given (and itself) """
with_deps{C<:Card}( w::Solved, cc::Array{C} ) ::Array{Plan} = _plans( Plan[], w, cc)
export with_deps

with_deps{T<:Trouble}( w::T, other... ) ::Array{Plan} = Plan[w]


"""( target-array[plans], plan, Array[cards] ) -> target-array[plans]
    where target-array[plans] is preallocated Array{Plan} to store result """
function _plans{W<:Plan,C<:Card}( ww::Array{W}, w::Plan, cc::Array{C} ) ::Array{Plan}
    push!(ww, w)
    adrs = need(w)::Array
    _plans( ww, adrs, cc)
end


"""( target-array[plans], Array[addresses], Array[cards] ) -> target-array[plans]
    where target-array[plans] is preallocated Array{Plan} to store result"""
function _plans{W<:Plan,S<:AbstractString,C<:Card}( ww::Array{W}, adrs::Array{S}, cc::Array{C} ) ::Array{Plan}
    isempty(adrs) && return ww
    next_adr::S = shift!(adrs)
    _plans( _plans( ww::Array{W}, next_adr, cc)::Array{Plan}, adrs::Array{S}, cc)
end


"""( target-array[plans], address, Array[cards] ) -> target-array[plans]
    where target-array is preallocated Array{Plan}  to store result"""
function _plans{W<:Plan,S<:AbstractString,C<:Card}( ww::Array{W}, adr::S, cc::Array{C}) ::Array{Plan}
    push!(ww, plan( cc, adr))
end


"need(w::Plan) returns -> Array[needs] of dependencies of next sub level"
need(w::Trouble)::Array = AbstractString[]
export need

function need{S<:Solved}(w::S)::Array
    deps = w.need(w)
    t = typeof(deps)
    if t <: Array 
        if eltype(deps)<:AbstractString
            return deps
        else
            error("Eltype of $deps must be AbstractString")
        end    
    elseif t<: Void
        return AbstractString[]
    else
        error("$w: need() must return Array, but was returned $t : $deps")
    end
end

"""Result represents returned value or exception

    How to return error:

    Result()

    Result(ErrorException(\"no\"))

    How to return value:
    
    Result(\"Ok\")
    
    Result(123)

"""
type Result
 some::Bool
 val

 Result() = new( false, ErrorException("Some was wrong...") )
 Result(e::Exception) = new( false, e)
 Result(c) = new( true, c)
end

some( r::Result)::Bool = r.some
iserr( r::Result)::Bool = !r.some

import Base.get
val(r::Result) = r.some ? r.val : error("$r has no value")
val(r::Result, deflt) = r.some ? r.val : deflt
err( r::Result ) = !r.some ? r.val : error("$r has no error")
err( r::Result, deflt ) = !r.some ? r.val : deflt

export Result,some,val,err



"plan -> Result"
prepare(t::Trouble, other...)::Result = Result(ErrorException("Trouble cant be prepared: $t"))
export prepare

function prepare{ P<:Plan, C<:Card }( plans::Vector{P}, cards::Vector{C} )::Array{Result}
 map( _->prepare( _, cards), plans)
end

"Calls plan.prepare(plan, deps) and return it wrapped into Result"
function prepare{ S<:Solved, C<:Card }( plan::S, cards::Vector{C} )::Result
    ok = ready( plan)
    ok==nothing && return Result(ErrorException("ready() return $ok (not implemented for $plan ?)"))
    typeof(ok)<:Bool || return Result( ErrorException("ready() return $ok. Must return ::Bool"))
    ok && return Result( plan)
    anddeps = with_deps( plan, cards)
    shift!( anddeps)
    if !isempty( anddeps)
        errors = prepare( anddeps, cards) |> _->filter( iserr, _) |> collect
        !isempty( errors) && return Result( ErrorException( "Errors: $errors - while prepare $plan"))
    end    
    needs = need(plan)
    try rv = plan.prepare( plan, needs)
        return Result(rv)
    catch e
        return Result( ErrorException( "Error in prepare().\nPlan:$plan.\n Exception:$e\n $( catch_stacktrace())" ))
    end
end

"""Checks if plan is ready ::Bool"""
function ready(plan::Plan ) 
    try rv = plan.ready( plan)
        if typeof(rv)<:Bool
            return rv
        else
            error("ready() must return ::Bool. ( rv = $rv , plan = $plan )")
        end    
        
    catch e
        error("Error while call plan.ready() on $plan : $e ")
    end
end    
export ready


"""
Return readable object for open() it
"""
function readable( plan::Plan)
    plan.readable( plan)
end
export readable

"""
Return writable TMP object for open( ... ,\"w\") it
"""
function writable_tmp( plan::Plan)
    plan.writable_tmp( plan)
end
export writable_tmp

"Creates sample p::Plan object for c::Card object based on his 'sample' field."
sample_plan{C<:Card}( c::C ) = plan( c, c.sample.addr )::Plan
export sample_plan

"Returns all sample dependencies as array of p::Plans from c::Card and his 'sample' field."
function sample_deps{C<:Card}( c::C ) ::Array{Plan}
 p1 = sample_plan(c)
 deps = with_deps( p1, [c])
end
export sample_deps

"Returns sample needs of sub-level"
function sample_need{C<:Card}(c::C) ::Array{String}
 p1 = sample_plan(c)
 needs = need(p1)
end 
export sample_need

"Does! prepare on sample."
function sample_prepare{C<:Card}(c::C) ::Result
 p1 = sample_plan(c)
 rv = prepare(p1, [c])
end 
export sample_prepare 


"Does ready() on sample"
function sample_ready{C<:Card}(c::C) ::Bool
 p1 = sample_plan(c)
 rv = ready(p1)
end
export sample_ready


"Does open for read on 'sample' field of card"
function sample_readable{C<:Card}(c::C)
 p1 = sample_plan(c)
 rv = readable(p1)
end
export sample_readable



"Does open for wtite on 'sample' field of card"
function sample_writable_tmp{C<:Card}(c::C)
 p1 = sample_plan(c)
 rv = writable_tmp(p1)
end
export sample_writable_tmp

 
end # module













