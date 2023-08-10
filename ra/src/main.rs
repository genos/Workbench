use clap::Parser;
use image::{ImageBuffer, Luma, Rgb};
use ra::{build, Eval};
use rand::{rngs::StdRng, SeedableRng};

#[derive(Parser)]
struct Args {
    /// Seed for PRNG
    #[arg(short, long)]
    seed: u64,
    /// RGB color picture (instead of greyscale)
    #[arg(short, long)]
    rgb: bool,
    /// Max recursion depth for building the picture
    #[arg(short, long, default_value_t = 10)]
    depth: u64,
    /// Height of picture
    #[arg(long, default_value_t = 512)]
    height: u32,
    /// Width of picture
    #[arg(long, default_value_t = 512)]
    width: u32,
}

fn main() {
    let args = Args::parse();
    let name = format!(
        "{}_{}_{}_{}_{}.png",
        args.seed,
        args.depth,
        args.height,
        args.width,
        if args.rgb { "rgb" } else { "grey" }
    );
    let to_real = |i: u32, n: u32| f64::from(i) / f64::from(n);
    let to_intensity = |f: f64| unsafe { (127.5 + (127.5 * f)).to_int_unchecked::<u8>() };
    let mut rng = StdRng::seed_from_u64(args.seed);
    if args.rgb {
        let r = build(&mut rng, args.depth);
        let g = build(&mut rng, args.depth);
        let b = build(&mut rng, args.depth);
        ImageBuffer::from_fn(args.width, args.height, |x, y| {
            let (xx, yy) = (to_real(x, args.width), to_real(y, args.height));
            Rgb([
                to_intensity(r.eval(xx, yy)),
                to_intensity(g.eval(xx, yy)),
                to_intensity(b.eval(xx, yy)),
            ])
        })
        .save(name)
        .expect("Unable to save color image");
    } else {
        let e = build(&mut rng, args.depth);
        ImageBuffer::from_fn(args.width, args.height, |x, y| {
            let (xx, yy) = (to_real(x, args.width), to_real(y, args.height));
            Luma([to_intensity(e.eval(xx, yy))])
        })
        .save(name)
        .expect("Unable to save greyscale image");
    }
}
