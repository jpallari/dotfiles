#!/usr/bin/env node
/*
Convert a CDK app manifest file to a stack dependency tree in Mermaid format.
*/
const fs = require('fs');

function dependencyTreeInMermaid(data) {
  const names = {};
  const links = [];

  for (const [key, value] of Object.entries(data.artifacts)) {
    if (value.type === 'aws:cloudformation:stack') {
      names[key] = value.displayName.split('/').pop();
      for (const dependency of value.dependencies) {
        if (!dependency.endsWith('.assets')) {
          links.push([key, dependency]);
        }
      }
    }
  }

  const linksStr = links.map(([key, value]) => {
    const keyName = names[key];
    const valueName = names[value];
    return `  ${keyName} --> ${valueName}`;
  }).join('\n')
  return [
    'flowchart TD',
    linksStr,
  ].join('\n')
}

function readJsonFile(fileName) {
  const file = fs.readFileSync(fileName, 'utf8');
  return JSON.parse(file);
}

function main() {
  const jsonFile = readJsonFile(process.argv[2]);
  console.log(dependencyTreeInMermaid(jsonFile));
}

if (typeof require !== 'undefined' && require.main === module) {
  main();
}
