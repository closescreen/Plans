"""
Develop with testing:

importall Plans 

    c1 = sample(\"total10.txt\") |> 

    card( r\"total(\\d+)\.txt\", File, Plain ) |> 

need() do p; # first argument will be a p::Solved

         [ \"source\$i.txt\" for i in 1:parse( Int, p.addr[1]) ] 

    end

c1 |> sample_plan

c1 |> sample_deps



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

abstract Source

"File data source"
immutable File<:Source
end

"Command data source"
immutable Command<:Source
end

export Source, File, Command

"Codec determines how to compress|decompress or other encoding|decoding input and output"
abstract Codec

"Not compressed data"
abstract Plain<:Codec

"Gzip compressed data"
abstract Gzip<:Codec
export Codec, Plain, Gzip


"""Card construct objects described re::Regex, typeof(source), typeof(codec), functions: need(), prepare(), iter()"""
immutable Card{ S<:Source, C<:Codec }
    sample::Sample
    regex::Regex
    sourcetype::Type{S}
    codectype::Type{C}
    need::Function
    prepare::Function
    ready::Function    
    openit::Function
    iterit::Function
end
export Card


card{S<:Source,C<:Codec}(
    sample::Sample, 
    regex::Regex, 
    sourcetype::Type{S}, 
    codectype::Type{C}; 
    need::Function=(w)->nothing, 
    prepare::Function=(w)->nothing, 
    ready::Function=(w)->nothing, 
    openit::Function=(w)->nothing, 
    iterit::Function=(w)->nothing
)::Card = 
    Card( sample, regex, sourcetype, codectype, need, prepare, ready, openit, iterit )
export card


"Sample(\"lala\") |> card( re, ...)"
card{S<:Source,C<:Codec}( 
    regex::Regex, 
    sourcetype::Type{S}, 
    codectype::Type{C} 
)::Function = 
    (s::Sample)-> card( s, regex, sourcetype, codectype )


"Card(...) |> ready() do w .... end # --> new Card (with 'need' field)"
ready(f::Function)::Function = 
    (c::Card)->
        card( c.sample, c.regex, c.sourcetype, c.codectype, 
            need=c.need, prepare=c.prepare, ready=f, openit=c.openit, iterit=c.iterit)
export ready


"Card(...) |> need() do w .... end # --> new Card (with 'need' field)"
need(f::Function)::Function = 
    (c::Card)->
        card( c.sample, c.regex, c.sourcetype, c.codectype, 
            need=f, prepare=c.prepare, ready=c.ready, openit=c.openit, iterit=c.iterit)
export need


"Card(...) |> prepare() do w .... end # --> new Card (with 'need' field)"
prepare(f::Function)::Function = 
    (c::Card)->
        card( c.sample, c.regex, c.sourcetype, c.codectype, 
            need=c.need, prepare=f, ready=c.ready, openit=c.openit, iterit=c.iterit)
export prepare


"Card(...) |> opeit() do w .... end # --> new Card (with 'need' field)"
opeit(f::Function)::Function = 
    (c::Card)->
        card( c.sample, c.regex, c.sourcetype, c.codectype, 
            need=c.need, prepare=c.prepare, ready=c.ready, openit=f, iterit=c.iterit)
export openit


"Card(...) |> iterit() do w .... end # --> new Card (with 'need' field)"
iterit(f::Function)::Function = 
    (c::Card)->
        card( c.sample, c.regex, c.sourcetype, c.codectype, 
            need=c.need, prepare=c.prepare, ready=c.ready, openit=c.openit, iterit=f)
export iterit


abstract Plan


"Plan describes matched address, typeof(source), typeof(codec), need() function"
immutable Solved{S<:Source,C<:Codec}<:Plan
    addr::RegexMatch
    sourcetype::Type{S}
    codectype::Type{C}
    need::Function
    prepare::Function
    ready::Function
    openit::Function
    iterit::Function
end


"prepares Solved object"
_solved{S<:Source,C<:Codec}(
    addr::RegexMatch, 
    sourcetype::Type{S}, 
    codectype::Type{C}; 
    need::Function=(w)->nothing, 
    prepare::Function=(w)->nothing, 
    ready::Function=(w)->nothing,
    openit::Function=(w)->nothing,
    iterit::Function=(w)->nothing ) = 
        Solved( addr, sourcetype, codectype, need, prepare, ready, openit, iterit)


"Used for unmatched addresses"
immutable Trouble<:Plan
    addr::AbstractString
end

"prepares Troube object"
_trouble(addr::AbstractString) = Trouble(addr)

export Plan, Solved, Trouble



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
    if (m=match(c.re, adr ))!=nothing
        _solved( m, c.source, c.codec, 
            need=c.need, prepare=c.prepare, ready=c.ready, openit=c.openit, iterit=c.iterit)
    else
        _trouble(adr)
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
    adrs = _need(w)::Array
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


"plan -> Array[needs]"
_need(w::Trouble)::Array = AbstractString[]

function _need{S<:Solved}(w::S)::Array
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

"prepares sample p::Plan object for c::Card object based on his 'sample' field."
sample_plan{C<:Card}( c::C ) = plan( c, c.sample.addr )::Plan
export sample_plan

"prepares sample array of p::Plans from c::Card and his 'sample' field."
function sample_deps{C<:Card}( c::C ) ::Array{Plan}
 p1 = plan( c, c.sample.addr )  
 deps = with_deps( p1, [c])
end
export sample_deps



end # module