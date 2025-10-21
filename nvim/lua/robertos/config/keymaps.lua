local s = vim.keymap.set
s('n', '<leader>lz', '<cmd>Lazy<cr>', { desc = 'Open Lazy UI' })
s('n', '<leader>;', '<cmd>nohlsearch<cr>', { desc = 'Stop Highlighting search' })
s('n', '<C-t>c', '<cmd>tabc<cr>', { desc = 'Close tab' })
s('i', 'jk', '<ESC>', { desc = 'Quick escape insert mode' })
