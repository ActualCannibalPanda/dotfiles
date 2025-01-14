return {
  'nvim-telescope/telescope.nvim',
  tag = '0.1.8',
  dependencies = { 'nvim-lua/plenary.nvim', 'debugloop/telescope-undo.nvim' },
  config = function()
    require('telescope').setup({
      extensions = {
        undo = {
          -- telescope-undo config
        },
      },
    })
    require('telescope').load_extension('undo')
  end,
  keys = {
    {
      '<leader>u',
      '<cmd>Telescope undo<cr>',
      desc = 'Open Telescope undo',
    },
    {
      '<leader>ff',
      '<cmd>Telescope find_files<cr>',
      desc = 'Telescope find files',
    },
    {
      '<leader>f<space>',
      '<cmd>Telescope git_files<cr>',
      desc = 'Telescope git files',
    },
    {
      '<leader>fg',
      '<cmd>Telescope live_grep<cr>',
      desc = 'Telescope live grep',
    },
    {
      '<leader>fb',
      '<cmd>Telescope buffers<cr>',
      desc = 'Telescope buffers',
    },
    {
      '<leader>fh',
      '<cmd>Telesope help_tags<cr>',
      desc = 'Telescope help tags',
    },
  },
}
