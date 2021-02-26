


defmodule ECompleto.Utils do

  def put_new(map, k, v) do
    %{map | k => v}
  end

  def subsets([]), do: [[]]
  def subsets([x|xs]) do
    xss = subsets(xs)
    (for y <- xss, do: [x|y]) ++ xss
  end


  def subsets_stream([]), do: [[]]
  def subsets_stream([x|xs]) do

    xss = subsets_stream(xs)
    Stream.concat([(Stream.map(xss, &( [x|&1] ))), xss])
  end

  def element_combinations([]) do
    [[]]
  end

  def element_combinations([first_list | element_lists]) do
    xss = element_combinations(element_lists)
    first_list |> Enum.flat_map(
      fn element ->
        (for y <- xss, do: [element|y])
      end
    )
  end

  def to_string_list(list, sep) do
    list |> Enum.map(&( &1 |> String.Chars.to_string ))
    |> Enum.join(sep)
  end

end
