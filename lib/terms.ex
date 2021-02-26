

defmodule ECompleto.Terms.FTerm do
  @doc """
  Defines a functional term with a functor and a list of arguments.
  """
  @enforce_keys [:functor]
  defstruct [:functor , arguments: [], skolem: false, type: :term]


  defimpl String.Chars, for: ECompleto.Terms.FTerm do
    def to_string(term) do
      args = term.arguments
        |> Enum.map(&( &1 |> String.Chars.to_string ))
        |> Enum.join(", ")

      f = if term.skolem do
        "#{term.functor |> String.Chars.to_string }'"
      else
        term.functor |> String.Chars.to_string
        pred = "#{term.functor}"
        if ( pred  |> String.starts_with?("http://")) do
          "<#{pred}>"
        else
          pred
        end
      end
      if term.arguments |> length > 0 do
        "#{f}(#{args})"
      else
        f
      end
    end
  end

end

defmodule ECompleto.Terms.Variable do
  @doc """
  Defines a variable with possibly an index for standarization appart purpuses.
  """
  @enforce_keys [:name]
  defstruct [:name , index: 0, type: :variable]

  defimpl String.Chars, for: ECompleto.Terms.Variable do
    def to_string(v) do
      if v.index > 0 do
        "#{v.name}_#{v.index}"
      else
        v.name
      end
    end
  end

end

defmodule ECompleto.Terms do

  @doc """
  Creates a term that represents a function functor and arguments.

  """
  def new_term(functor, arguments) do
    %ECompleto.Terms.FTerm{functor: functor, arguments: arguments}
  end

  @doc """
  Creates a Skolem term.

  """
  def new_skterm(functor, arguments) do
    %ECompleto.Terms.FTerm{functor: functor, arguments: arguments, skolem: true}
  end

  @doc """
  Creates a variable with an index
  """
  def new_var(vname, vindex) do
    %ECompleto.Terms.Variable{
      name: vname,
      index: vindex
    }
  end

  @doc """
  Creates a variable with zero index
  """
  def new_var(vname) do
    new_var(vname,0)
  end

  @doc """
  Creates a term that represents constant i.e. a term without arguments.

  """
  def new_constant(constant) when is_atom(constant) do
    new_term(constant,[])
  end


  @doc """
  Tells if the term is a Skolem term or not.
  """
  def is_skterm?(x=%{}) do
    Map.get(x,:skolem)
  end

  def is_skterm?(_) do
    false
  end


end
