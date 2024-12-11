return {
	"akinsho/toggleterm.nvim",
	version = "*",
	opts = {},
	config = function()
		if vim.uv.os_uname().sysname == "Windows_NT" then
			if vim.fn.executable("powershell") then
				vim.opt.shell = "powershell"
			else
				vim.opt.shell = "pwsh"
			end
			vim.opt.shellcmdflag =
				"-NoLogo -NoProfile -ExecutionPolicy RemoteSigned -Command [Console]::OutputEncoding=[System.Text.Encoding]::UTF8;"
			vim.opt.shellredir = "RedirectStandardOutput %s -NoNewWindow -Wait"
			vim.opt.shellpipe = "2>&1 | Out-File -Encoding UTF8 %s; exit $LastExitCode"
			vim.opt.shellquote = ""
			vim.opt.shellxquote = ""
		end
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
