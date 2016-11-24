"""
1. Create wanted number of cards:
    Any card::Card object will describe file, it properties and dependencies. 

        importall Plans

        card_t1 = Card(r\"text\\d+\.txt\", File, Plain) ::Card

        card_g1 = Card(r\"text\\d+\.gz\", File, Gzip) ::Card

        cards = [card_t1, card_g1] ::Array


2. You may test any filename to match to cards array 
    And you will have w::Plan object (::Solved or ::Trouble) 

    p1 = plan( cards, \"text1.txt\" ) ::Plan

3. If you got w::Plan object, then you may get all dependencies of w

    p1_all = with_deps(w1, cards) ::Array   # will contain ::Trouble elements if was unmatched addresses

"""
module Plans

abstract Source

"File data source"
immutable File<:Source
    addr::AbstractString
end

"Command data source"
immutable Command<:Source
    addr::Cmd
end

export Source, File, Command

"Codec determines how to compress|decompress or other encoding|decoding input and output"
abstract Codec

"Not compressed data"
abstract Plain<:Codec

"Gzip compressed data"
abstract Gzip<:Codec

export Codec, Plain, Gzip


"""Card construct objects described re::Regex, typeof(source), typeof(codec), need() function"""
immutable Card{F<:Function, S<:Source, C<:Codec }
    need::F
    re::Regex
    source::S
    codec::Type{C}
end
export Card

abstract Plan
export Plan

Card{S<:Source,C<:Codec}( re::Regex, source::S, codec::Type{C}) = Card( (w::Plan)->nothing, re, source, codec )

"Plan describes matched address,  typeof(source), typeof(codec), need() function"
immutable Solved{F<:Function,S<:Source,C<:Codec}<:Plan
    need::F
    addr::RegexMatch
    source::S
    codec::Type{C}
end
export Solved

"Used for unmatched addresses"
immutable Trouble<:Plan
    addr::AbstractString
end


"for do-syntax call"
Solved{S<:Source, C<:Codec}(addr::RegexMatch, source::S, codec::Type{C}) =
    Solved(()->nothing, addr, source, codec)




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
    if (m=match(c.re, adr ))!=nothing
        Solved(c.need, m, c.source, c.codec)
    else
        Trouble(adr)
    end
end



"""Recursive find all planss depended from given (and itself) """
with_deps{C<:Card}( w::Solved, cc::Array{C} ) ::Array{Plan} = plans( Plan[], w, cc)
export with_deps
with_deps{T<:Trouble}( w::T, other... ) ::Array{Plan} = Plan[w]


"""( target-array[plans], plan, Array[cards] ) -> target-array[plans]
    where target-array[plans] is preallocated Array{Plan} to store result """
function plans{W<:Plan,C<:Card}( ww::Array{W}, w::Plan, cc::Array{C} ) ::Array{Plan}
    push!(ww, w)
    adrs = _need(w)::Array
    plans( ww, adrs, cc)
end
export plans


"""( target-array[plans], Array[addresses], Array[cards] ) -> target-array[plans]
    where target-array[plans] is preallocated Array{Plan} to store result"""
function plans{W<:Plan,S<:AbstractString,C<:Card}( ww::Array{W}, adrs::Array{S}, cc::Array{C} ) ::Array{Plan}
    isempty(adrs) && return ww
    next_adr::S = shift!(adrs)
    plans( plans( ww::Array{W}, next_adr, cc)::Array{Plan}, adrs::Array{S}, cc)
end


"""( target-array[plans], address, Array[cards] ) -> target-array[plans]
    where target-array is preallocated Array{Plan}  to store result"""
function plans{W<:Plan,S<:AbstractString,C<:Card}( ww::Array{W}, adr::S, cc::Array{C}) ::Array{Plan}
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


end # module