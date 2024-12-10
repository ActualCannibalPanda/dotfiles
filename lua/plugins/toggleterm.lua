return {
	"akinsho/toggleterm.nvim",
	version = "*",
	opts = {},
	config = function()
		require("toggleterm").setup()
		local Terminal = require("toggleterm.terminal").Terminal
		local lazygit = Terminal:new({
			cmd = "lazygit",
			hidden = true,
			direction = "float",
		})

		function _lazygit_toggle()
			lazygit:toggle()
		end

		vim.keymap.set(
			"n",
			"<leader>gg",
			"<cmd>lua _lazygit_toggle()<cr>",
			{ desc = "Open LazyGit", noremap = true, silent = true }
		)
		vim.keymap.set("n", "<leader>tt", function()
			vim.cmd([[ ToggleTermToggleAll ]])

			local buffers = vim.api.nvim_list_bufs()

			local toggleterm_exists = false
			for _, buf in ipairs(buffers) do
				local buf_name = vim.api.nvim_buf_get_name(buf)
				if buf_name:find("toggleterm#") then
					toggleterm_exists = true
					break
				end
			end

			if not toggleterm_exists then
				vim.cmd([[ exe 1 . "ToggleTerm direction=float" ]])
			end
		end, { desc = "Open terminal", noremap = true })
	end,
}
