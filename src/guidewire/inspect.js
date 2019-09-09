export function constructorName(value) {
  return value && typeof value === 'object'
    ? `: ${value.constructor.name}${value instanceof Array ? `[${value.length}]` : ''}`
    : '';
}

export function callerReference() {
  return new Error()
    .stack
    .split('\n')[2] // eslint-disable-line no-magic-numbers
    .match(/[-/A-za-z_.]+:\d+/)[0];
}

export function inspect(obj) {
  const messages = Object.entries(obj)
    .map(([name, value]) => [name, constructorName(value), JSON.stringify(value) || String(value)])
    .map(([name, constructor, value]) => `${name}${constructor} ⟶ ${value}`);
  messages.unshift(callerReference());
  let message = messages.join('\n');
  console.warn(message);
  return message;
}


const inspect_inline = (obj) => {
  const iCallerReference = new Error().stack
    .split('\n')[2]
    .match(/[-/A-za-z_.]+:\d+/)[0]; // eslint-disable-line no-magic-numbers
  const iConstructorName = value => (value && typeof value === 'object'
      ? `: ${value.constructor.name}${value instanceof Array ? `[${value.length}]` : ''}`
      : ''
  );
  const iMessages = Object.entries(obj)
    .map(([name, value]) => [name, iConstructorName(value), JSON.stringify(value) || String(value)])
    .map(([name, constructor, value]) => `${name}${constructor} ⟶ ${value}`);
  iMessages.unshift(iCallerReference);
  console.warn(iMessages.join('\n'));
}; // todo pschaaf Remove this debugging code
