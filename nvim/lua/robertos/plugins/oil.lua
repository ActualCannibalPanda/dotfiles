return {
  'stevearc/oil.nvim',
  dependencies = { { 'echasnovski/mini.icons', opts = {} } },
  event = { 'VimEnter */*,.*', 'BufNew */*,.*' },
  opts = {
    default_file_explorer = true,
    view_options = {
      show_hidden = true,
    },
  },
  keys = {
    {
      '<localleader>o',
      function()
        require('oil').toggle_float()
      end,
      desc = 'Open Oil',
      mode = { 'n' },
    },
  },
}
