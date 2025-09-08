return {
  'neovim/nvim-lspconfig',
  opts = {
    server = {
      lua_ls = {},
      rust_analyzer = {},
    },
  },
  keys = {
    {
      '<F2>',
      vim.lsp.buf.rename,
      desc = 'LSP Rename',
    },
  },
}
