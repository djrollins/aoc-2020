import * as io from "https://deno.land/std@0.83.0/io/mod.ts";
import * as path from "https://deno.land/std@0.83.0/path/mod.ts";

const inputFile = path.join(Deno.cwd(), Deno.args[1]);
const fileReader = await Deno.open(inputFile);

let iter = io.readLines(fileReader);

let arrivalTime: number = await iter.next().then((x) => parseInt(x.value));
let timetable: string[] = await iter.next()
  .then((x) => x.value.split(","));

let part1 = timetable
  .map(bus => parseInt(bus))
  .filter(isFinite)
  .map(bus => {
    let time = bus;
    while (time <= arrivalTime) {
      time += bus;
    }
    return [bus, time];
  })
  .sort((bus1, bus2) => bus1[1] - bus2[1])
  .map(bus => bus[0] * (bus[1] - arrivalTime))[0];

console.log(part1);

let part2 = timetable.map((bus) => {
  let i = parseInt(bus);
  return [i, i];
});

const iterate = (timetable: number[][]) => {
  for (let x = 0; x <= timetable.length; ++x) {
    if (isNaN(timetable[x][1])) continue;

    for (let y = x + 1; y < timetable.length; ++y) {
      console.log(x, timetable[x], y, timetable[y]);
      if (isNaN(timetable[y][1])) continue;

      while (timetable[x][1] < timetable[y][1] - y) {
        timetable[x][1] += timetable[x][0];
      }

      if (timetable[x][1] != timetable[y][1] - y) {
        timetable[y][1] += timetable[y][0];
        x = 0;
        break;
      }
    }
  }
};

iterate(part2);

console.log(part2[0][1]);
