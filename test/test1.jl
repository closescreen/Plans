using Plans

sample("11.txt")

sample("11.txt") |>
card(r"(\d+)\.txt")

c1 = sample("11.txt") |>
    card(r"(\d+)\.txt") |>
    prepare_will() do plan,needs
        n = plan.addr[1]
        open( "$addr.TMP", "w") do wio
            println( wio, "hello $n")
        end
        mv( "$addr.TMP", "$addr")
    end


c1 |> sample_ready # false

c1 |> sample_prepare

c1 |> sample_ready # true

c1 |> sample_writable_tmp 

c1 |> sample_readable

