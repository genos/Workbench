use nannou::prelude::*;

fn main() {
    nannou::sketch(view).run();
}

fn view(app: &App, frame: Frame) {
    let draw = app.draw();
    draw.background().color(BEIGE);
    let mut points = vec![
        Vec2::new(-250.0, -200.0),
        Vec2::new(250.0, -200.0),
        Vec2::new(0.0, 250.0),
    ];
    let mut p = {
        // random point inside triangle https://stackoverflow.com/questions/19654251
        let [a, b, c] = points[0..3] else { unreachable!() };
        let (r1, r2): (f32, f32) = (random(), random());
        (1.0 - r1.sqrt()) * a + (r1.sqrt() * (1.0 - r2)) * b + (r1.sqrt() * r2) * c
    };
    points.push(p);
    for _ in 0..10_000 {
        let i = random_range(0, 3);
        p = p + 0.5 * (points[i] - p);
        points.push(p);
    }
    for point in &points {
        draw.x_y(point.x, point.y)
            .ellipse()
            .w_h(3.0, 3.0)
            .color(TEAL);
    }
    draw.to_frame(app, &frame).unwrap();
}
