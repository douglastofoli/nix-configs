local wk = require("which-key")

return {
  "elixir-tools/elixir-tools.nvim",
  version = "*",
  event = { "BufReadPre", "BufNewFile" },
  config = function()
    local elixir = require("elixir")
    local elixirls = require("elixir.elixirls")

    elixir.setup {
      credo = {},
      elixirls = {
        enable = true,
        settings = elixirls.settings {
          dialyzerEnabled = true,
          enableTestLenses = true,
          fetchDeps = true,
          suggestSpecs = false,
        },
        on_attach = function(client, bufnr)
          wk.register({
            ["ce"] = {
              name = "elixir",
              p = { "<cmd>ElixirToPipe<cr>", "Convert to pipe operator" },
              P = { "<cmd>ElixirFromPipe<cr>", "Convert from pipe operator" },
              m = { "<cmd>ElixirExpandMacro<cr>", "Expand macro and display content" },
              t = { function() vim.lsp.codelens.run() end, "Run test lenses" },
            },
          }, { prefix = "<leader>" })
        end,
      },
    }
  end,
  dependencies = {
    "nvim-lua/plenary.nvim",
  },
}
