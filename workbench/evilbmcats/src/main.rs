extern crate rand;
extern crate reqwest;
extern crate select;

use rand::{thread_rng, Rng};
use reqwest::{get, Response, Url};
use select::document::Document;
use select::predicate::*;
use std::io::Read;

fn text_image_href(doc: &Document) -> Option<(String, String, String)> {
    let tweets = doc.find(Name("p").and(Class("tweet-text"))).into_selection();
    let images = doc.find(Name("img").and(Attr("style", "width: 100%; top: -0px;")))
        .into_selection();
    if tweets.len() == 0 || images.len() == 0 || tweets.len() != images.len() {
        None
    } else {
        let n = thread_rng().gen_range(0, tweets.len());
        let tweet = tweets.clone().iter().nth(n);
        let text =
            tweet.and_then(|t| t.text().find("pic.twitter").map(|p| t.text()[..p].to_string()));
        let image = images.iter().nth(n).and_then(|i| i.attr("src").map(|s| s.to_string()));
        let href = tweet.and_then(|t| {
            t.descendants()
                .find(|d| Name("a").matches(d))
                .and_then(|a| a.attr("href").map(|h| h.to_string()))
        });
        match (text, image, href) {
            (Some(t), Some(i), Some(h)) => Some((t, i, h)),
            _ => panic!("Unable to get one of tweet, image, or href"),
        }
    }
}

fn main() {
    let doc = {
        let url = Url::parse("https://twitter.com/search?q=from:%40evilbmcats")
            .expect("Unable to construct url");
        let mut body = String::new();
        let mut result: Response = get(url).expect("No response from url");
        result.read_to_string(&mut body).expect("Unable to read url");
        Document::from(body.as_str())
    };
    match text_image_href(&doc) {
        None => panic!("Unable to pull things from doc"),
        Some((text, image, href)) => {
            println!("**{}**\n![Black Metal Cats]({})\n*[@evilbmcats on Twitter]({})*",
                     text,
                     image,
                     href)
        }
    }
}
