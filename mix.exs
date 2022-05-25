defmodule Ecompleto.MixProject do
  use Mix.Project

  def project do
    [
      app: :ecompleto,
      version: "0.1.0",
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:rdf, git: "https://github.com/rdf-elixir/rdf-ex.git", tag: "v0.9.2"},
      {:exprof, "~> 0.2.0"},
      {:flow, "~> 1.0"},
      # Code Quality
      {:credo, "~> 1.1", only: [:dev, :test], runtime: false},
      # Code quality check.
      {:dialyxir, "~> 1.0", only: [:dev], runtime: false},
      # Benchmarking
      {:benchee, "~> 1.0", only: :dev}
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
    ]
  end
end
