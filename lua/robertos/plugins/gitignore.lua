return {
  'wintermute-cell/gitignore.nvim',
  keys = {
    {
      '<leader>gi',
      function()
        local root_dir = vim.fs.dirname(require('lspconfig').util.root_pattern()())
        require('gitignore').generate(root_dir)
      end,
    },
  },
}
