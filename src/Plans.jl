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
sample( addr::AbstractString) = Sample( addr)
export sample
 

immutable StringCard
    sample::AbstractString
    regex::Regex
    need::Expr
    prepare::Expr
    ready::Expr
    readable::Expr
    iter::Expr
end    
export StringCard

import Base.show
function show( io::IO, sc::StringCard)
 need = sc.need == DEFAULT_NEED ? s"DEFAULT_NEED" : sc.need
 prepare = sc.prepare == DEFAULT_PREPARE ? s"DEFAULT_PREPARE" : sc.prepare
 ready = sc.ready == DEFAULT_READY ? s"DEFAULT_READY" : sc.ready
 readable = sc.readable == DEFAULT_READABLE ? s"DEFAULT_READABLE" : sc.readable
 iter = sc.iter == DEFAULT_ITER ? s"DEFAULT_ITER" : sc.iter
 
 print( io, """\n$(sc|>typeof) --------------------------
sample: $(sc.sample)\nregex: $(sc.regex)
need: $need
prepare: $prepare
ready: $ready
readable: $readable
iter: $iter
-------------------------------------------""")
end

const DEFAULT_NEED = """(p)-> nothing"""|>parse
const DEFAULT_PREPARE = """(p)-> nothing"""|>parse
const DEFAULT_READY = s"""(p)-> ( str=string(p); ismatch( r"\.gz(?=ip)$"i, str) ? filesize( str)>20 : filesize( str)>0 ) """|>parse
const DEFAULT_READABLE = s""" (p)-> ( str=string(p); ismatch( r"\.gz(?=ip)?$"i, str) ? `zcat $str` : str ) """|>parse
const DEFAULT_ITER = """(p)-> eachline( readable( p))"""|>parse

"""Parse <expr> for infornative name <name> and check result and return it. 
Throw error if wrong."""
function parsefor( name::AbstractString, expr::AbstractString)::Expr
 try 
    p = parse(expr)::Expr 
    p.head == :incomplete && error("""Incomplete "$name" expression.""")
    return p 
 catch e 
    error("$e\n while parse:\n $expr \n\n $( catch_stacktrace())") 
 end 
end

"Not parse. Only check"
function parsefor( name::AbstractString, expr::Expr)::Expr
 try 
    expr.head == :incomplete && error("""Incomplete "$name" expression.""")
    return expr 
 catch e 
    error("$e\n while checking:\n $expr \n\n $( catch_stacktrace())") 
 end 
end



"Constructs StringCard from incomplete params"
string_card(
    sample::AbstractString,
    regex::AbstractString,
    regexflags = "",
    need = DEFAULT_NEED,
    prepare = DEFAULT_PREPARE,
    ready = DEFAULT_READY,
    readable = DEFAULT_READABLE,
    iter = DEFAULT_ITER
    ) = StringCard(
            sample,
            try Regex(regex, regexflags) catch e error(
                """$e\n  regex:'$regex' regexflags:'$regexflags' \n\n  $(catch_stacktrace())""") end,
            
            parsefor( "need", need),
            parsefor( "prepare", prepare ),
            parsefor( "ready", ready),
            parsefor( "readable", readable),
            parsefor( "iter", iter) )


string_card(
    sample::AbstractString,
    regex::Regex,
    need = DEFAULT_NEED,
    prepare = DEFAULT_PREPARE,
    ready = DEFAULT_READY,
    readable = DEFAULT_READABLE,
    iter = DEFAULT_ITER
    ) = StringCard(
            sample,
            regex,
            parsefor( "need", need),
            parsefor( "prepare", prepare ),
            parsefor( "ready", ready),
            parsefor( "readable", readable),
            parsefor( "iter", iter) )


"""
sample( \"lala1.txt\") |> card( \"lala\") # StringCard with sample and regex

sample( \"lala1.txt\") |> card( \"lala\\\\d\\.txt\", \"i\") # the same and with flags
"""
card( re::AbstractString ) = (s::Sample)->string_card( s.addr, re)
card( re::AbstractString, flags::AbstractString ) = (s::Sample)->string_card( s.addr, re, flags) 
export card


need_will( need::AbstractString) = (sc::StringCard)->string_card( sc.sample, sc.regex, need)
export need_will


prepare_will( prepare::AbstractString) = 
    (sc::StringCard)->string_card( sc.sample, sc.regex, sc.need, prepare)
export prepare_will


ready_will( ready::AbstractString) = 
    (sc::StringCard)->string_card( sc.sample, sc.regex, sc.need, sc.prepare, ready)
export ready_will


readable_will( readable::AbstractString ) = 
    (sc::StringCard)->string_card( sc.sample, sc.regex, sc.need, sc.prepare, sc.ready, readable)
export readable_will


iter_will( iter::AbstractString) = 
    (sc::StringCard)->
        string_card( sc.sample, sc.regex, sc.need, sc.prepare, sc.ready, sc.readable, iter )
export iter_will



"""Card construct objects described regex::Regex functions: need(), prepare(), iter()"""
immutable Card
    string_card::StringCard
    sample::Sample
    regex::Regex
    need::Function
    prepare::Function
    ready::Function    
    readable::Function
    iter::Function
end
export Card

show( io::IO, c::Card) = print( io, """$Card (compiled)
string_card:$(c.string_card)
""")


"""
Constricts compiled Card from StringCard
"""
compile( sc::StringCard ) = 
    Card(
        sc, 
        Sample( sc.sample),
        sc.regex,
        try eval(sc.need)::Function catch e error("$e\n $( catch_stacktrace()) \n while eval text: $(sc.need)") end,
        try eval(sc.prepare)::Function catch e error("$e\n $( catch_stacktrace()) \n while eval text: $(sc.prepare)") end,
        try eval(sc.ready)::Function catch e error("$e\n $( catch_stacktrace()) \n while eval text: $(sc.ready)") end,
        try eval(sc.readable)::Function catch e error("$e\n $( catch_stacktrace()) \n while eval text: $(sc.readable)") end,
        try eval(sc.iter)::Function catch e error("$e\n $( catch_stacktrace()) \n while eval text: $(sc.iter)") end
    )
export compile


abstract Plan


"Plan describes matched address, typeof(source), typeof(codec), need() function"
immutable Solved<:Plan
    card::Card
    addr::RegexMatch
end

show( io::IO, s::Solved) = print( io, "$Solved. addr: $(s.addr)", s.card)


"Used for unmatched addresses"
immutable Trouble<:Plan
    addr::AbstractString
end

show( io::IO, t::Trouble) = print( io, "$Trouble. addr: $(t.addr)")

export Plan, Solved, Trouble


import Base.string
"Returns address of plan as string"
string(p::Solved) = p.addr.match
string(t::Trouble) = t.addr
export string

"""
Returns tmp-name for writing it.

Sample:

\"\$( myplan|>tmp )\" # usage inside string interpolation

"""
tmpname(p::Solved) = join( (p.addr.match, "TMP"), '.')
tmpname(t::Trouble) = join( (p.addr, "TMP"), '.')
export tmpname


"(Array{cards}, address) -> plan"
function plan{C<:Card}( cc::Array{C}, adr::AbstractString )::Plan
    for c in cc
        w = plan( c, adr)
        typeof(w) <: Solved && return w
    end
    Trouble(adr)
end
export plan


"(card, address)->plan"
function plan( c::Card, adr::AbstractString)::Plan
    if (m=match( c.regex, adr)) != nothing
        Solved( c, m)
    else
        Trouble( adr)
    end
end

"Auto compile StringCard->Card and then call plan()"
plan( sc::StringCard, adr::AbstractString)::Plan = plan( card(sc), adr)



"""Recursive find all plans depended from given (and itself) """
with_deps{C<:Card}( plan::Solved, cc::Array{C} ) ::Array{Plan} = _plans( Plan[], plan, cc)
export with_deps


with_deps{T<:Trouble}( t::T, other... ) ::Array{Plan} = Plan[t]


"""( target-array[plans], plan, Array[cards] ) -> target-array[plans]
    where target-array[plans] is preallocated Array{Plan} to store result """
function _plans{P<:Plan,C<:Card}( pp::Array{P}, p::Plan, cc::Array{C} ) ::Array{Plan}
    push!(pp, p)
    adrs = need(p)::Array
    _plans( pp, adrs, cc)
end


"""( target-array[plans], Array[addresses], Array[cards] ) -> target-array[plans]
    where target-array[plans] is preallocated Array{Plan} to store result"""
function _plans{P<:Plan,S<:AbstractString,C<:Card}( pp::Array{P}, adrs::Array{S}, cc::Array{C} ) ::Array{Plan}
    isempty(adrs) && return pp
    next_adr::S = shift!(adrs)
    _plans( _plans( pp::Array{P}, next_adr, cc)::Array{Plan}, adrs::Array{S}, cc)
end


"""( target-array[plans], address, Array[cards] ) -> target-array[plans]
    where target-array is preallocated Array{Plan}  to store result"""
function _plans{P<:Plan,S<:AbstractString,C<:Card}( pp::Array{P}, adr::S, cc::Array{C}) ::Array{Plan}
    push!(pp, plan( cc, adr))
end


"need(p::Plan) returns -> Array[needs] of dependencies of next sub level"
need(::Trouble)::Array = AbstractString[]
export need


function need{S<:Solved}(p::S)::Array
    deps = p.card.need(p)
    t = typeof(deps)
    if t <: Array 
        if eltype(deps)<:AbstractString
            return deps
        else
            error("Eltype of $deps must be AbstractString")
        end    
    elseif t<: Dict{Symbol}
        if deps|>values|>eltype <:AbstractString
            return deps
        else
            error("Values type of dict $deps must be <:AbstractString ($(deps|>values|>eltype)). Plan:$plan.")
        end
    elseif t<: Void
        return AbstractString[]
    else
        error("$p: need() must return Array, but was returned $t : $deps")
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
val( r::Result) = r.some ? r.val : error("$r has no value")
val( r::Result, deflt) = r.some ? r.val : deflt
err( r::Result ) = !r.some ? r.val : error("$r has no error")
err( r::Result, deflt ) = !r.some ? r.val : deflt

export Result,some,val,err



"plan -> Result"
prepare( t::Trouble, other...)::Result = Result(ErrorException("Trouble cant be prepared: $t"))
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
    try rv = plan.card.prepare( plan, needs)
        return Result(rv)
    catch e
        return Result( ErrorException( "$e - in prepare().\n Plan:$plan.\n $(catch_stacktrace()) \n prepare: $(plan.card.string_card.prepare)" ))
    end
end


"""Checks if plan is ready ::Bool"""
function ready(plan::Plan ) 
    try rv = plan.card.ready( plan)
        if typeof(rv)<:Bool
            return rv
        else
            error("ready() must return ::Bool. ( rv = $rv , plan = $plan )")
        end    
        
    catch e
        error("$e - while call plan.card.ready() on $plan. \n $(catch_stacktrace()) \n ready: $(plan.card.string_card.ready)")
    end
end    
export ready


"""
Returns readable object for open() it
"""
function readable( plan::Plan)
    ready(plan) ? plan.card.readable( plan) : error("Can't return readable: $plan is not ready. Check ready(plan).")
end
export readable


"""
Returns iterator from readable
"""
function iter(plan::Plan)
    ready( plan ) ? plan.card.iter( plan) : error( "Can't return iter: $plan is not ready. Check ready(plan).")
end
export iter


"Creates sample p::Plan object for c::Card object based on his 'sample' field."
sample_plan{C<:Card}( c::C ) = plan( c, c.sample.addr )::Plan
export sample_plan

sample_plan{SC<:StringCard}( sc::SC ) = sample_plan( card(sc))::Plan

"Returns all sample dependencies as array of p::Plans from c::Card and his 'sample' field."
function sample_deps{C<:Card}( c::C ) ::Array{Plan}
 p1 = sample_plan(c)
 deps = with_deps( p1, [c])
end
export sample_deps

sample_deps{SC<:StringCard}( sc::SC ) :: Array{Plan} = sample_deps( card( sc))

"Returns sample needs of sub-level"
function sample_need{C<:Card}(c::C) ::Array{String}
 p1 = sample_plan(c)
 needs = need(p1)
end 
export sample_need

sample_need{SC<:StringCard}(sc::SC) ::Array{String} = sample_need( card( sc))

"Does! prepare on sample."
function sample_prepare{C<:Card}(c::C) ::Result
 p1 = sample_plan(c)
 rv = prepare(p1, [c])
end 
export sample_prepare 

sample_prepare{SC<:StringCard}(sc::SC) :: Result = sample_prepare( card( sc))

"Does ready() on sample"
function sample_ready{C<:Card}(c::C) ::Bool
 p1 = sample_plan(c)
 rv = ready(p1)
end
export sample_ready

sample_ready{SC<:StringCard}(sc::SC) = sample_ready( sc)

"Does open for read on 'sample' field of card"
function sample_readable{C<:Card}(c::C)
 p1 = sample_plan(c)
 rv = readable(p1)
end
export sample_readable

sample_readable{SC<:StringCard}(sc::SC) = sample_readable( card( sc))


"Card matched for any pattern any behave as file"
const DEFAULT_STRING_CARD = sample( "anydir/anyfile.ext " ) |> card( s".*" )
"Compiled DEFAULT_STRING_CARD"
const DEFAULT_CARD = compile(DEFAULT_STRING_CARD)
 
end # module













