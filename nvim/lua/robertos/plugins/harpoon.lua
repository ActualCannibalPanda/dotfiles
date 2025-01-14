return {
  'ThePrimeagen/harpoon',
  dependencies = { 'nvim-lua/plenary.nvim' },
  keys = {
    {
      '<leader>hf',
      function()
        require('harpoon.mark').add_file()
      end,
      desc = 'Add file to harpoon',
    },
    {
      '<leader>hh',
      function()
        require('harpoon.ui').toggle_quick_menu()
      end,
      desc = 'Open Harpoon quick menu',
    },
    {
      '<leader>hn',
      function()
        require('harpoon.ui').nav_next()
      end,
      desc = 'Nav to next file in Harpoon',
    },
    {
      '<leader>hp',
      function()
        require('harpoon.ui').nav_prev()
      end,
      desc = 'Nav to previous file in Harpoon',
    },
    {
      '<leader>hc',
      function()
        require('harpoon.ui').nav_file(vim.v.count)
      end,
    },
  },
}
