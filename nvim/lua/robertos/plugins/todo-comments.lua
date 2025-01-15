return {
  'folke/todo-comments.nvim',
  dependencies = {
    'nvim-lua/plenary.nvim',
    'nvim-telescope/telescope.nvim',
    'ibhagwan/fzf-lua',
    'folke/trouble.nvim',
  },
  -- TODO: thing
  config = function()
    require('todo-comments').setup()

    vim.keymap.set('n', ']t', function()
      require('todo-comments').jump_next()
    end, { desc = 'Next todo comment' })

    vim.keymap.set('n', '[t', function()
      require('todo-comments').jump_next()
    end, { desc = 'Previous todo comment' })

    vim.keymap.set('n', '<leader>td', '<cmd>TodoTelescope<cr>', { desc = 'Open TodoTelescope' })
  end,
}
