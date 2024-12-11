return {
	"stevearc/resession.nvim",
	config = function()
		local resession = require("resession")
		resession.setup({
			autosave = {
				enabled = true,
				interval = 60,
				notify = true,
			},
		})

		vim.api.nvim_create_autocmd("VimEnter", {
			callback = function()
				if vim.fn.argc(-1) == 0 then
					resession.load(vim.fn.getcwd(), {
						dir = "dirsession",
						silence_errors = true,
					})
				end
			end,
			nested = true,
		})
		vim.api.nvim_create_autocmd("VimLeavePre", {
			callback = function()
				resession.save(vim.fn.getcwd(), {
					dir = "dirsession",
					notify = false,
				})
			end,
		})
	end,
	keys = {
		{
			"<leader>ss",
			function()
				require("resession").save()
			end,
			desc = "Save session",
			mode = "n",
		},
		{
			"<leader>sl",
			function()
				require("resession").load()
			end,
			desc = "Load session",
			mode = "n",
		},
		{
			"<leader>sd",
			function()
				require("resession").delete()
			end,
			desc = "Delete session",
			mode = "n",
		},
	},
}
