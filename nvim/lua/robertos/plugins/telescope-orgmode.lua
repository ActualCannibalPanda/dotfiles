return {
  'nvim-orgmode/telescope-orgmode.nvim',
  event = 'VeryLazy',
  dependencies = {
    'nvim-orgmode/orgmode',
    'nvim-telescope/telescope.nvim',
  },
  config = function()
    require('telescope').load_extension('orgmode')

    vim.keymap.set('n', '<leader>r', require('telescope').extensions.orgmode.refile_heading)
    vim.keymap.set('n', '<leader>fh', require('telescope').extensions.orgmode.search_headings)
    vim.keymap.set('n', '<leader>li', require('telescope').extensions.orgmode.insert_link)
    vim.api.nvim_create_autocmd('FileType', {
      pattern = 'org',
      group = vim.api.nvim_create_augroup('orgmode_telescope_nvim', { clear = true }),
      callback = function()
        vim.keymap.set('n', '<leader>or', require('telescope').extensions.orgmode.refile_heading)
      end,
    })
  end,
}
