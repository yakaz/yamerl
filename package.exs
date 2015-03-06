defmodule Yamerl.Mixfile do
  use Mix.Project

  def project do
    [
      app: :yamerl,
      version: "0.3.1-1",
      description: description,
      package: package,
      deps: []
    ]
  end

  defp description do
    """
    YAML 1.2 parser in pure Erlang
    """
  end

  defp package do
    [
      files: ~w(src doc include testsuite configure.ac rebar.config README.md AUTHORS COPYING),
      contributors: ["Yakaz", "Jean-Sébastien Pédron"],
      licenses: ["BSD 2-Clause"],
      links: %{
        "GitHub" => "https://github.com/yakaz/yamerl",
        "Doc" => "https://github.com/yakaz/yamerl/tree/master/doc"
      }]
   end
end
