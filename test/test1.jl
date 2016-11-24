using Base.Test

importall Wants

card_t1 = Card(r"text\d+\.txt", File, Plain)
@test typeof(card_t1) <:Card

card_g1 = Card(r"text\d+\.gz", File, Gzip)
@test typeof(card_g1) <: Card

cards = [card_t1, card_g1]

w1 = want( cards, "text1.txt" )
@test typeof(w1)<:Want

want_and_deps = and_deps(w1, cards)
@test typeof(allw) <: Array



