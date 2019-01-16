open Reprocessing;

type xDirection = Left(float, float) | Right(float, float);
type yDirection = Up(float, float) | Down(float, float);

type coord = {
  x: xDirection,
  y: yDirection,
};

type color = {
  r: int,
  g: int,
  b: int,
  a: int,
};

type ball = {
  age: int,
  rad: float,
  bounce: float,
  center: coord,
  color: color,
};

type balls = list(ball);

type state = {
  balls: balls,
};

let unwrapX = d => {
  switch(d) {
    | Left(x, _) => x
    | Right(x, _) => x
  }
}

let unwrapY = d => {
  switch(d) {
    | Up(y, _) => y
    | Down(y, _) => y
  }
}

let coordToTuple = c => (int_of_float(unwrapX(c.x)), int_of_float(unwrapY(c.y)));

/* Constants */
let width = 1000.0;
let height = 500.0;
let g = 0.9;
let f = 0.9;
let maxAge = 1000;
let fadeAge = 950;

let addBall = (state: state, x, y) => {
  {
    balls: [
      {
        age: 0,
        rad: Random.float(10.0) +. 5.0,
        bounce: Random.float(0.58) +. 0.4,
        color: {
          r: Random.int(255),
          g: Random.int(255),
          b: Random.int(255),
          a: 255,
        },
        center: {
          x: Random.bool() ? Right(float_of_int(x), Random.float(8.0)) : Left(float_of_int(x), Random.float(8.0)),
          y: Down(float_of_int(y), 0.0),
        },
      },
      ...state.balls,
    ],
  }
};

let setup = (env) => {
  Env.size(~width=int_of_float(width), ~height=int_of_float(height), env);

  { balls: [] }
};

let draw = (state: state, env) => {
  /* Draw background */
  Draw.fill(Utils.color(~r=0, ~g=0, ~b=0, ~a=255), env);
  Draw.rect(~pos=(0, 0), ~width=int_of_float(width), ~height=int_of_float(height), env);

  let balls = state.balls
    |> List.map((ball) => {
        let { age, rad, bounce, center, color } = ball;
        let newAge = age + 1;

        {
          ...ball,
          age: newAge,
          color: {
            ...color,
            a: age >= fadeAge ? int_of_float(255.0 *. (1.0 -. (float_of_int(age - fadeAge) /. float_of_int(maxAge - fadeAge)))) : color.a,
          },
          center: {
            x: {
              switch (center.x) {
                | Right(x, vx) => Right(x +. vx, unwrapY(center.y) >= (height -. rad) ? max(0.0, vx *. f) : vx)
                | Left(x, vx) => Left(x -. vx, unwrapY(center.y) >= (height -. rad) ? max(0.0, vx *. f) : vx)
              }
            },
            y: {
              switch (center.y) {
                | Down(y, vy) when (y +. vy >= (height -. rad)) => Up(height -. rad, vy *. bounce)
                | Down(y, vy) => Down(y +. vy, vy +. g)
                | Up(y, vy) when (y -. vy <= rad || vy -. g <= 0.0) => Down(y, vy)
                | Up(y, vy) => Up(y -. vy, vy -. g)
              }
            }
          }
        }
      })
    |> List.filter(({ center, rad, age }) => {
        let oobX = switch (center.x) {
          | Right(x, _) => x -. rad > width
          | Left(x, _) => x +. rad < 0.0
        };

        let oobY = switch (center.y) {
          | Down(y, _) => y -. rad > height
          | Up(y, _) => y +. rad < 0.0
        };

        !oobX && !oobY && age < maxAge
      });

  /* Draw balls */
  List.iter(({ center, rad, color }) => {
    Draw.fill(Utils.color(~r=color.r, ~g=color.g, ~b=color.b, ~a=color.a), env);
    Draw.ellipse(~center=coordToTuple(center), ~radx=int_of_float(rad), ~rady=int_of_float(rad), env);
  }, balls);

  { balls: balls }
};

let mouseDown = (state: state, env) => {
  let (x, y) = Env.mouse(env);

  addBall(state, x, y)
};

run(~setup, ~draw, ~mouseDown, ());
