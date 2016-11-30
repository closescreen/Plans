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

    p1_all = with_deps(p1, cards) ::Array   # will contain ::Trouble elements if was unmatched addresses

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


"""Card construct objects described re::Regex, typeof(source), typeof(codec), functions: need(), create(), iter()"""
immutable Card{ S<:Source, C<:Codec }
    sample::Sample
    re::Regex
    source::Type{S}
    codec::Type{C}
    need::Function
    create::Function
    iter::Function
end
export Card

"Constructor with wrapped parameters: Need, Create, Iter."
Card{S<:Source,C<:Codec}(sample::Sample, re::Regex, source::Type{S}, codec::Type{C} )::Card = 
    Card( sample, re, source, codec, (w)->nothing, (w)->nothing, (w)->nothing )

"Sample(\"lala\") |> Card( re, ...)"
Card{S<:Source,C<:Codec}( re::Regex, source::Type{S}, codec::Type{C} )::Function = 
    (s::Sample)->Card( s, re, source, codec, (w)->nothing, (w)->nothing, (w)->nothing )


"Card(...) |> need() do w .... end # --> new Card (with 'need' field)"
need(f::Function)::Function = (c::Card)->Card(c.sample, c.re, c.source, c.codec, f, c.create, c.iter)
export need
 

"Card(...) |> create() do want ... end # ---> new Card with 'create' field"
create(f::Function)::Function = (c::Card)->Card(c.sample, c.re, c.source, c.codec, c.need, f, c.iter)
export create


"Card(...) |> iter() do want .... end # --> new Card with 'iter' field"
iter(f::Function)::Function = (c::Card)->Card(c.sample, c.re, c.source, c.codec, c.need, c.create, f)
export iter



abstract Plan

"Plan describes matched address,  typeof(source), typeof(codec), need() function"
immutable Solved{S<:Source,C<:Codec,F1<:Function}<:Plan
    addr::RegexMatch
    source::Type{S}
    codec::Type{C}
    need::F1
    create::Function
    iter::Function
end

"Creates Solved object"
_solved{S<:Source,C<:Codec}(addr::RegexMatch, source::Type{S}, codec::Type{C}, 
            need::Function=(w)->nothing, 
                create::Function=(w)->nothing, 
                    iter::Function=(w)->nothing ) = Solved(addr,source,codec,need,create,iter)

"Used for unmatched addresses"
immutable Trouble<:Plan
    addr::AbstractString
end

"Creates Troube object"
_trouble(addr::AbstractString) = Trouble(addr)

export Plan, Solved, Trouble

Solved{S<:Source, C<:Codec}(addr::RegexMatch, source::Type{S}, codec::Type{C}, 
                                need::Function=(w)->nothing,
                                    create::Function=(w)->nothing,
                                        iter::Function=(w)->nothing) =
                                            Solved( addr, source, codec, need.need, create.create, iter.iter )



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
        _solved( m, c.source, c.codec, c.need)
    else
        _trouble(adr)
    end
end



"""Recursive find all plans depended from given (and itself) """
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