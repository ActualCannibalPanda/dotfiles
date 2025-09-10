return {
  'neovim/nvim-lspconfig',
  dependencies = {
    { 'ms-jpq/coq_nvim', branch = 'coq' },
    { 'ms-jpq/coq.artifacts', branch = 'artifacts' },
  },
  init = function()
    vim.g.coq_settings = {
      auto_start = true,
    }
  end,
  config = function()
    local coq = require('coq')
    vim.lsp.config('lua_ls', coq.lsp_ensure_capabilities())
    vim.lsp.enable('lua_ls')

    vim.keymap.set('n', '<F2>', vim.lsp.buf.rename)
  end,
}
