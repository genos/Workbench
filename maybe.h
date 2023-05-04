// maybe.h, with help from
// yapb-soc.blogspot.com/2012/08/implementing-maybe-monad-in-c++.html

#ifndef _MAYBE_H
#define _MAYBE_H

#include <memory>
#include <ostream>
#include <stdexcept>

namespace maybe {
    template<class X>
    using Maybe = std::shared_ptr<X>;

    template<class X>
    auto Just(const X& x) -> Maybe<X> { return std::make_shared<X>(x); }

    // Specific
    template<class X>
    auto Nothing() -> Maybe<X> { return nullptr; }

    // Generic, rely on implicit casting
    auto Nothing() -> std::nullptr_t { return nullptr; }

    template <class X>
    auto isJust(const Maybe<X>& m) -> bool { return m != Nothing(); }

    template <class X>
    auto isNothing(const Maybe<X>& m) -> bool { return m == Nothing(); }

    template <class X>
    auto MaybeError() -> X {
        throw std::domain_error("Maybe Error");
        return X{};
    }

    template<class X>
    auto fromJust(const Maybe<X>& m) -> X {
        return isJust(m) ? *m : MaybeError<X>();
    }

    template<class X>
    auto operator<<(std::ostream& os, const Maybe<X>& m) -> std::ostream& {
        if (isJust(m)) {
            os << "Just " << fromJust(m);
        } else {
            os << "Nothing";
        }
        return os;
    }

    // fmap f (Just x) = Just (f x)
    // fmap f Nothing  = Nothing        where f: X -> Y
    template<class F, class X,
             class Y=decltype(std::declval<F>()(std::declval<X>()))>
    auto fmap(const F& f, const Maybe<X>& m) -> Maybe<Y> {
        return isJust(m) ? Just(f(fromJust(m))) : Nothing();
    }

    // (own) alias for fmap (can't use Haskell's <$>)
    template<class F, class X,
             class Y=decltype(std::declval<F>()(std::declval<X>()))>
    auto operator^(const F& f, const Maybe<X>& m) -> Maybe<Y> {
        return fmap(f, m);
    }

    // mbind f (Just x) = f x
    // mbind f Nothing  = Nothing       where f: X -> Maybe<Y>
    template<class F, class X,
             class MaybeY=decltype(std::declval<F>()(std::declval<X>()))>
    auto mbind(const F& f, const Maybe<X>& m) -> MaybeY {
        return isJust(m) ? f(fromJust(m)) : Nothing();
    }

    // alias for mbind (like Haskell)       
    template<class F, class X,
             class MaybeY=decltype(std::declval<F>()(std::declval<X>()))>
    auto operator>>=(const Maybe<X>& m, const F& f) -> MaybeY {
        return mbind(f, m);
    }

}  // namespace maybe

#endif  // _MAYBE_H
