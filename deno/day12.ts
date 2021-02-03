import { readLines } from "https://deno.land/std@0.83.0/io/mod.ts";
import * as path from "https://deno.land/std@0.83.0/path/mod.ts";

const inputFile = path.join(Deno.cwd(), Deno.args[1]);
const fileReader = await Deno.open(inputFile);

type Coord = { x: number; y: number };

type CardinalDirection = "N" | "E" | "S" | "W";
type RotationalDirection = "R" | "L";

const move_cardinal = (
  coord: Coord,
  direction: CardinalDirection,
  distance: number,
): Coord => {
  let { x, y } = coord;
  switch (direction) {
    case "N":
      return { x, y: y + distance };
    case "E":
      return { y, x: x + distance };
    case "S":
      return { x, y: y - distance };
    case "W":
      return { y, x: x - distance };
  }
};

abstract class Commandable {
  abstract _forward(amount: number): void;
  abstract _move_cardinal(direction: CardinalDirection, amount: number): void;
  abstract _rotate_right_90(): void;

  _rotate_right(angle: number) {
    if (angle === 0) {
      return;
    }

    this._rotate_right_90();
    this._rotate_right(angle - 90);
  }

  _rotate(direction: RotationalDirection, angle: number) {
    if (direction === "L" && angle != 180) {
      angle = (angle + 180) % 360;
    }

    this._rotate_right(angle);
  }
  _isCardinal(string: string): string is CardinalDirection {
    return "NSWE".includes(string);
  }

  _isRotation(string: string): string is RotationalDirection {
    return "LR".includes(string);
  }

  command(str: string) {
    let command = str[0];
    let amount = parseInt(str.slice(1));

    if (this._isCardinal(command)) {
      this._move_cardinal(command, amount);
    } else if (this._isRotation(command)) {
      this._rotate(command, amount);
    } else if (command === "F") {
      this._forward(amount);
    }
  }
}

class Waypoint extends Commandable {
  ship: Coord = { x: 0, y: 0 };
  waypoint: Coord = { x: 10, y: 1 };

  _rotate_right_90() {
    this.waypoint = {
      x: this.waypoint.y,
      y: -this.waypoint.x,
    };
  }

  _forward(amount: number): void {
    this.ship = {
      x: this.ship.x + this.waypoint.x * amount,
      y: this.ship.y + this.waypoint.y * amount,
    };
  }

  _move_cardinal(direction: CardinalDirection, amount: number): void {
    this.waypoint = move_cardinal(this.waypoint, direction, amount);
  }
}

class Ship extends Commandable {
  coord: Coord = { x: 0, y: 0 };
  #direction: CardinalDirection = "E";

  _move_cardinal(direction: CardinalDirection, distance: number) {
    this.coord = move_cardinal(this.coord, direction, distance);
  }

  _forward(distance: number) {
    this._move_cardinal(this.#direction, distance);
  }

  _rotate_right_90() {
    switch (this.#direction) {
      case "N":
        this.#direction = "E";
        break;
      case "E":
        this.#direction = "S";
        break;
      case "S":
        this.#direction = "W";
        break;
      case "W":
        this.#direction = "N";
        break;
    }
  }
}

const manhatten_distance = ({ x, y }: Coord): number =>
  Math.abs(x) + Math.abs(y);

const ship = new Ship();
const waypoint = new Waypoint();
console.log(waypoint);
for await (const line of readLines(fileReader)) {
  ship.command(line);
  waypoint.command(line);
  console.log(waypoint);
}

console.log(manhatten_distance(ship.coord));
console.log(manhatten_distance(waypoint.ship));
