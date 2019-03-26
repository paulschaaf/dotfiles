/* eslint-disable import/no-extraneous-dependencies */
const chalk = require('chalk');

const errorSeverity = 2;
const fileNameWidth = 44;
const ruleIDWidth = 33;
const ruleViolationCountWidth = 2;

// Functions
const colorize = (isError, str) => (isError ? chalk.red(str) : chalk.yellow(str));

const ruleIdUrl = (id) => {
  const [ruleLabel, lintPlugin = ''] = id.split('/')
    .reverse();
  const ruleName = ruleLabel.replace(/ /g, '');
  switch (lintPlugin) {
    case '':
      return `https://eslint.org/docs/rules/${ruleName}`;
    case 'import':
      return `https://github.com/benmosher/eslint-plugin-import/blob/master/docs/rules/${ruleName}.md`;
    case 'jsx-a11y':
      return `https://github.com/evcohen/eslint-plugin-jsx-a11y/blob/master/docs/rules/${ruleName}.md`;
    case 'no-only-tests':
      return 'https://www.npmjs.com/package/eslint-plugin-no-only-tests';
    case 'react':
      return `https://github.com/yannickcr/eslint-plugin-react/blob/master/docs/rules/${ruleName}.md`;
    default:
      return `https://www.google.com/search?q=eslint+${id}`;
  }
};

const violationMap = new Map();
violationMap.map = (block) => {
  const result = [];
  violationMap.forEach((value, key) => result.push(block(value, key)));
  return result;
};

violationMap.errors = 0;
violationMap.warnings = 0;

const recordViolation = (isError, id) => {
  const colorizedName = colorize(isError, id);
  if (isError) {
    violationMap.errors++;
  } else {
    violationMap.warnings++;
  }
  const count = violationMap.has(colorizedName) ? violationMap.get(colorizedName).count : 0;
  violationMap.set(colorizedName, {
    id,
    count: count + 1,
  });
};

const violations = () => violationMap
  .map(({ id, count }, ruleName) => ({ id, count, ruleName }))
  .sort((a, b) => (a.count <= b.count ? 1 : -1)) // descending by frequency
  .map(({ id, count, ruleName }) => `${String(count).padStart(ruleViolationCountWidth)}: ${ruleName} ${ruleIdUrl(id)}`);

const violationsTable = () => [
  chalk.bold(`${violationMap.errors} error(s), ${violationMap.warnings} warning(s)`),
  ...violations(),
];

const cwdLength = process.cwd().length + 1;

// Main export

module.exports = (results) => {
  const output = [];

  results
    .filter(fileResult => fileResult.messages.length > 0)
    .forEach(({ filePath, messages }) => {
      const relativePath = filePath.substr(cwdLength);

      messages.forEach(({ fatal, severity, line, column, ruleId, message }) => {
        const isError = fatal || severity === errorSeverity;
        const location = `${relativePath}:${line}:${column}`;
        const id = ruleId.padEnd(ruleIDWidth);
        recordViolation(isError, id);

        output.push(`${location.padEnd(fileNameWidth)}  ${id}  ${colorize(isError, message)}`);
      });
      output.push('');
    });

  return [...output, ...violationsTable(), ''].join('\n');
};
