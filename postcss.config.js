// This file configures PostCSS to purge the unused Tailwind classes.

const plugins = [require("autoprefixer"), require("tailwindcss")];

/*

NOTE: I could not prevent purgeCss to purge my base and component classes.
I'll just disable it because there doesn't seem to be an alternative, except not using
Parcel in the build process.

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
*/

module.exports = { plugins };
