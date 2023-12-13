const path = require('path');

module.exports = {
  mode: 'production',
  entry: './index.ts', // Update this path to your actual entry file
  output: {
    filename: 'bundle.js', // Output bundle file name
    path: path.resolve(__dirname, 'dist'), // Output directory
  },
  resolve: {
    extensions: ['.tsx', '.ts', '.js'], // Resolve these extensions
  },
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: 'ts-loader',
        exclude: /node_modules/,
      },
    ],
  },
};
