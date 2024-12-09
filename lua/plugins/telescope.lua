return {
	"nvim-telescope/telescope.nvim",
	tag = "0.1.8",
	requires = { { "nvim-lua/plenary.nvim" } },
	keys = {
		{
			"<leader>ff",
			"<cmd>Telescope find_files<cr>",
			desc = "Telescope find files",
		},
		{
			"<leader>fg",
			"<cmd>Telescope live_grep<cr>",
			desc = "Telescope live grep",
		},
		{
			"<leader>fb",
			"<cmd>Telescope buffers<cr>",
			desc = "Telescope buffers",
		},
		{
			"<leader>fh",
			"<cmd>Telesope help_tags<cr>",
			desc = "Telescope help tags",
		},
	},
}
