defmodule ECompleto.DLGPBuilder do
  @moduledoc false

  import ECompleto.Program

  def get_line(code) do
    p = code |> load_program_from_text
    p.body |> Enum.at(0)
  end
end
