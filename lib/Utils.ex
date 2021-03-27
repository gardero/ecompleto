


defmodule ECompleto.Utils do

  @doc """
  puts a key and value into a map.
  """
  def put_new(map, k, v) do
    %{map | k => v}
  end

  @doc """
  buids a list of all the subsets (as lists) of a given list.
  """
  def subsets([]), do: [[]]
  def subsets([x|xs]) do
    xss = subsets(xs)
    (for y <- xss, do: [x|y]) ++ xss
  end


  @doc """
  buids a Stream with all the subsets (as lists) of a given list.
  """
  def subsets_stream([]), do: [[]]
  def subsets_stream([x|xs]) do

    xss = subsets_stream(xs)
    Stream.concat([(Stream.map(xss, &( [x|&1] ))), xss])
  end

  def element_combinations([]) do
    [[]]
  end
  @doc """
  takes a list of lits and builds all the combinations of lists taking from each list an element.
  """
  def element_combinations([first_list | element_lists]) do
    xss = element_combinations(element_lists)
    first_list |> Enum.flat_map(
      fn element ->
        (for y <- xss, do: [element|y])
      end
    )
  end

  @doc """
  builds a string with all the elements in the list separated by a defined separator.
  """
  def to_string_list(list, sep) do
    list |> Enum.map(&( &1 |> String.Chars.to_string ))
    |> Enum.join(sep)
  end

end
