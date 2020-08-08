// This file configures PostCSS to purge the unused Tailwind classes.

const tailwind = require("tailwindcss");

const plugins = [tailwind];

const is_dev = process.env.NODE_ENV === "development";

if (!is_dev) {
	const purgecss = require("@fullhuman/postcss-purgecss");

	const extractor = function (content) {
		return content.match(/[A-z0-9\-:\/]+/g) || [];
	};

	plugins.push(
		purgecss({
			content: ["src/*.elm"],
			defaultExtractor: extractor,
		})
	);
}

module.exports = { plugins };
