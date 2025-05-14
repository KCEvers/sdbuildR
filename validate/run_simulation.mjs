import fs from 'fs';
import path from 'path';

import { Model } from "simulation";
// import { table, plot } from "simulation-viz-console";
import { loadInsightMaker } from "simulation";
// Import Mersenne Twister as ES module
// import MersenneTwister from 'mersenne-twister';
import seedrandom from 'seedrandom';

// Directory containing .InsightMaker files
const directory = "C://Users//kevers1//Documents//PhD//sdbuildR//validate//models";

// Read all files in the directory
fs.readdir(directory, (err, files) => {
    if (err) {
        console.error("Could not list the directory.", err);
        process.exit(1);
    }

    files.forEach((file, index) => {
        if (path.extname(file) === '.InsightMaker') {
            const filePath = path.join(directory, file);
            console.log(`Processing file: ${filePath}`);
            // Call your function here with filePath as the argument
            // Example:
            processFile(filePath);
        }
    });
});


function processFile(filepath) {
    console.log(`Processing: ${filepath}`);

    try {
      // Read file
      console.log('Read model')
      const data = readFile(filepath);
      const m = loadInsightMaker(data);
      // console.log(m)

      // Modify to Euler
      m.algorithm = "Euler"
      // m.timeStep = .1
      console.log(m.algorithm) //  ("SolutionAlgorithm"))

      // Set seed
      console.log(m.globals)

      // Change timestep if too large and algorithm is RK4
      if (m.timeStep >= 1 & m.algorithm == "RK4"){
              m.timeStep = .1
      }
    //   // Add to global string
    // //   m.globals = "setRandSeed(123)\\n " + m.globals + "\\nsetRandSeed(123)";
    //   m.globals = m.globals + "\\nsetRandSeed(1380)";
    //   console.log(m.globals)

//      // Create a seedrandom generator using the same seed as in R
// const rng = seedrandom(123); // Use the same seed you used in R
// // Generate a series of random numbers
// for (let i = 0; i < 4; i++) {
//     console.log(rng()); // Outputs random numbers in the range [0, 1)
//   }

  // Initialize the generator with the same seed as R
    //  const generator = new MersenneTwister(123); // Seed is 123, matching R
// // Generate random numbers
// console.log(generator.random());  // First random number
// console.log(generator.random());  // Second random number
// console.log(generator.random());  // Third random number

      // Example in JavaScript if working with a simulation library
      // const seedrandom = require('seedrandom');
      // const rng = seedrandom(123);


      // Simulate
      let results = m.simulate();
      console.log('Simulated model')

      // Save
      // const outputFilePath = filepath.replace(/\.InsightMaker$/, '.json');
      const outputFilePath = filepath.replace(/\.InsightMaker$/, '_euler.json');
      console.log(outputFilePath);

      // Serialize object to JSON string
      const jsonString = JSON.stringify(results, null, 2); // `null, 2` for pretty print
      console.log('Created JSON string')
    //   console.log(jsonString)

      // Write JSON string to a file
      fs.writeFile(outputFilePath, jsonString, (err2) => {
        if (err2) {
          console.error('Error writing file:', err2);
        } else {
          console.log('File saved successfully!');
        }
      });

      return 1;
    } catch (err) {
        console.error("An error occurred while processing the file:", err);
        // throw err; // Re-throw the error if you want to handle it outside
    }

    // // table(results, Moose);
    // // plot(results, Moose);

}



function readFile(filepath) {
  try {
      const data = fs.readFileSync(filepath, 'utf8');
      return data;
  } catch (err) {
      console.error("An error occurred while reading the file:", err);
      throw err; // Re-throw the error if you want to handle it outside
  }
}

// cd Documents\PhD\sdbuildR\validate
// node run_simulation.mjs
