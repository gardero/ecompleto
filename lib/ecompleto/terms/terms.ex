defmodule ECompleto.Terms do
  @moduledoc """
  Creates a term that represents a function functor and arguments.
  """
  alias ECompleto.Terms.{
    FTerm,
    Variable
  }

  alias __MODULE__

  @type arg_term :: ECompleto.Terms.FTerm.t() | ECompleto.Terms.Variable.t()
  @type terms_list :: [] | [arg_term()]

  @spec new_term(String.t() | atom(), terms_list()) :: FTerm.t()
  def new_term(functor, arguments) do
    %FTerm{functor: functor, arguments: arguments}
  end

  @doc """
  Creates a Skolem term.
  ## Example
    iex> new_skterm("f", [])
    %FTerm{arguments: [], functor: "f", skolem: true, type: :term}
  """
  @spec new_skterm(String.t() | atom(), terms_list()) :: FTerm.t()
  def new_skterm(functor, arguments) do
    %FTerm{functor: functor, arguments: arguments, skolem: true}
  end

  @doc """
  Creates a variable with an index
  ## Example
    iex> ECompleto.Terms.new_var("x", 1)
    %ECompleto.Terms.Variable{index: 1, name: "x", type: :variable}
  """
  @spec new_var(String.t(), non_neg_integer()) :: Variable.t()
  def new_var(vname, vindex \\ 0) do
    %Variable{
      name: vname,
      index: vindex
    }
  end

  @doc """
  Creates a term that represents constant i.e. a term without arguments.
  ## Example
    ex> new_constant(:f)
    %FTerm{arguments: [], functor: :f, skolem: false, type: :term}

  """
  @spec new_constant(atom()) :: FTerm.t()
  def new_constant(constant) when is_atom(constant) do
    new_term(constant, [])
  end

  @doc """
  Tells if the term is a Skolem term or not.
  ## Example
    ex> %ECompleto.Terms.FTerm{arguments: [], functor: :f, skolem: false, type: :term} |> is_skterm?
    false
  """
  @spec is_skterm?(any()) :: boolean()
  def is_skterm?(x = %FTerm{}) do
    x.skolem
  end

  def is_skterm?(_) do
    false
  end
end
