defmodule Yamerl.Mixfile do
  use Mix.Project

  def project do
    [
      app: :yamerl,
      version: "0.3.3",
      description: description,
      package: package,
      deps: deps(),

      # Docs.
      name: "yamerl",
      source_url: "https://github.com/yakaz/yamerl",
      homepage_url: "https://github.com/yakaz/yamerl",
      docs: [
        extras: [
          "doc-md/features.md",
          "doc-md/installation.md",
          "doc-md/user-guide/recipe-error-handling.md",
          "doc-md/user-guide/recipe-basic-parsing.md",
          "doc-md/reference-manual/module-yamerl_constr.md",
          "doc-md/alternatives.md"
        ]
      ]
    ]
  end

  defp description do
    """
    YAML 1.2 parser in pure Erlang
    """
  end

  defp deps do
    [
      {:ex_doc, ">= 0.0.0", only: :dev}
    ]
  end

  defp package do
    [
      files: ~w(src doc-md include testsuite configure.ac rebar.config README.md AUTHORS COPYING),
      maintainers: ["Jean-Sébastien Pédron <jean-sebastien.pedron@dumbbell.fr>"],
      contributors: ["Yakaz", "Jean-Sébastien Pédron <jean-sebastien.pedron@dumbbell.fr>"],
      licenses: ["BSD 2-Clause"],
      links: %{
        "GitHub" => "https://github.com/yakaz/yamerl",
        "Doc" => "https://github.com/yakaz/yamerl/tree/master/doc"
      }
    ]
  end

end
