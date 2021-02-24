defmodule Ckini.MixProject do
  use Mix.Project

  def project do
    [
      app: :ckini,
      name: "Ckini",
      description: "A miniKanren implementation in Elixir",
      version: "0.1.0",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      package: package(),
      deps: deps(),
      source_url: "https://github.com/shouya/ckini"
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
      {:ex_doc, ">= 0.0.0", only: :dev, runtime: false}
    ]
  end

  defp package do
    [
      name: "ckini",
      files: ~w(lib mix.exs README.md LICENSE),
      licenses: ["GPL-2.0"],
      links: %{"GitHub" => "https://github.com/shouya/ckini"}
    ]
  end
end
