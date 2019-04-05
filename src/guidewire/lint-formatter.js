/* eslint-disable import/no-extraneous-dependencies */

// configurable settings
const ruleViolationCountWidth = 2;

const chalk = (() => {
  try {
    return require('chalk');
  } catch (e) {
  }
  return {
    bold: (str) => `** ${str} **`,
    red: (str) => `ERROR  ${str}`,
    yellow: (str) => `WARN   ${str}`,
  };
})();


// Functions
const ruleIdUrl = (id) => {
  const [ruleLabel, lintPlugin = ''] = id.split('/')
      .reverse();
  const ruleName = ruleLabel.replace(/ /g, '');
  switch (lintPlugin) {
    case '':  // no plugin--it's built-in
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

const colorize = (isError, str) => (isError ? chalk.red(str) : chalk.yellow(str));

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
  return colorizedName;
};

const violations = (idColumnWidth) => violationMap
    .map(({id, count}, ruleName) => ({id, count, ruleName}))
    .sort((a, b) => (a.count <= b.count ? 1 : -1)) // descending by frequency
    .map(({id, count, ruleName}) => `${String(count).padStart(ruleViolationCountWidth)}: ${ruleName.padEnd(idColumnWidth)}  ${ruleIdUrl(id)}`);

const violationsTable = (idColumnWidth) => [
  chalk.bold(`${violationMap.errors} error(s), ${violationMap.warnings} warning(s)`),
  ...violations(idColumnWidth),
    '',
    'To targetted disable use // eslint-disable-line <rule> or // eslint-disable-next-line <rule>',
];

const cwdLength = process.cwd().length + 1;

// Main export

const errorSeverity = 2;
let maxLocationLength = 0;
let maxViolationNameLength = 0;

module.exports = (results) => {
  const lines = [];

  results
      .filter(fileResult => fileResult.messages.length > 0)
      .forEach(({filePath, messages}) => {
        const relativePath = filePath.substr(cwdLength);

        messages.forEach(({fatal, severity, line, column, ruleId, message}) => {
          const isError = fatal || severity === errorSeverity;
          const location = `${relativePath}:${line}:${column}`;
          let violationName = recordViolation(isError, ruleId);

          maxLocationLength = Math.max(maxLocationLength, location.length);
          maxViolationNameLength = Math.max(maxViolationNameLength, violationName.length);

          lines.push([location, violationName, message]);
        });
        lines.push('');
      });

  const output = lines.map(([location, ruleId, message], index, arr) => location
        ? `${location.padEnd(maxLocationLength)}  ${ruleId.padEnd(maxViolationNameLength)}  ${message}`
        : ''
  );

  return [...output, ...violationsTable(maxViolationNameLength), ''].join('\n');
};
