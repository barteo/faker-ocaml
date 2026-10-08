export const cases = [
  ['vehicle', (f) => f.vehicle.vehicle()],
  ['manufacturer', (f) => f.vehicle.manufacturer()],
  ['model', (f) => f.vehicle.model()],
  ['type', (f) => f.vehicle.type()],
  ['fuel', (f) => f.vehicle.fuel()],
  ['vin', (f) => f.vehicle.vin()],
  ['color', (f) => f.vehicle.color()],
  ['vrm', (f) => f.vehicle.vrm()],
  ['bicycle', (f) => f.vehicle.bicycle()],
  ['vin/many', (f) => f.helpers.multiple(() => f.vehicle.vin(), { count: 30 })],
  ['fake', (f) => f.helpers.fake('{{vehicle.vehicle}} | {{vehicle.manufacturer}} | {{vehicle.model}} | {{vehicle.type}} | {{vehicle.fuel}} | {{vehicle.vin}} | {{vehicle.color}} | {{vehicle.vrm}} | {{vehicle.bicycle}}')],
];
