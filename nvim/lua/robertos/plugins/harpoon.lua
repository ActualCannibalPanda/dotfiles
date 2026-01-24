return {
  'ThePrimeagen/harpoon',
  dependencies = {
    'nvim-lua/plenary.nvim',
    'nvim-telescope/telescope.nvim',
  },
  branch = 'harpoon2',
  config = function()
    local harpoon = require('harpoon')

    harpoon:setup()

    local conf = require('telescope.config').values
    local function toggle_telescope(harpoon_files)
      local file_paths = {}
      for _, item in ipairs(harpoon_files) do
        table.insert(file_paths)
      end

      require('telescope.pickers')
        .new({}, {
          prompt_title = 'Harpoon',
          finder = require('telescope.finders').new_table({
            results = file_paths,
          }),
          previewer = conf.file_previewer({}),
          sorter = conf.generic_sorter({}),
        })
        :find()
    end

    local s = vim.keymap.set

    s('n', '<leader>hf', function()
      harpoon:list():add()
    end, { desc = 'Add file to harpoon' })
    s('n', '<leader>hh', function()
      harpoon.ui:toggle_quick_menu(harpoon:list())
    end, { desc = 'Toggle harpoon quick-menu' })
    s('n', '<leader>ht', function()
      toggle_telescope(harpoon:list())
    end, { desc = 'Toggle harpoon telescope' })
    s('n', '<leader>hj', function()
      harpoon:list():next()
    end, { desc = 'Next file in harpoon list' })
    s('n', '<leader>hk', function()
      harpoon:list():prev()
    end, { desc = 'Next file in harpoon list' })
    s('n', '<leader>1', function()
      harpoon:list():select(1)
    end, { desc = 'Next file in harpoon list' })
    s('n', '<leader>2', function()
      harpoon:list():select(2)
    end, { desc = 'Next file in harpoon list' })
    s('n', '<leader>3', function()
      harpoon:list():select(3)
    end, { desc = 'Next file in harpoon list' })
    s('n', '<leader>4', function()
      harpoon:list():select(4)
    end, { desc = 'Next file in harpoon list' })
    s('n', '<leader>5', function()
      harpoon:list():select(5)
    end, { desc = 'Next file in harpoon list' })
    s('n', '<leader>6', function()
      harpoon:list():select(6)
    end, { desc = 'Next file in harpoon list' })
    s('n', '<leader>7', function()
      harpoon:list():select(7)
    end, { desc = 'Next file in harpoon list' })
    s('n', '<leader>8', function()
      harpoon:list():select(8)
    end, { desc = 'Next file in harpoon list' })
    s('n', '<leader>9', function()
      harpoon:list():select(9)
    end, { desc = 'Next file in harpoon list' })
  end,
}
