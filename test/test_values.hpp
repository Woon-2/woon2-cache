#ifndef __test_values
#define __test_values

#include "LRUCache.hpp"

#include <forward_list>
#include <ranges>
#include <iterator>
#include <algorithm>
#include <memory>
#include <optional>
#include <utility>
#include <vector>

namespace test_values {

namespace detail {

template <class T>
class value_generator {};

template <class KeyT, class MappedT>
class value_generator< std::pair<const KeyT, MappedT> > {
public:
    using value_type = std::pair<const KeyT, MappedT>;

    static value_type gen() {
        return { value_generator<KeyT>::gen(), value_generator<MappedT>::gen() };
    }
};

#define __DEFINE_VALUE_GEN(type, ...)   \
template<>  \
class value_generator<type> {   \
public: \
    static type gen() { \
        return data_.get(); \
    }   \
        \
private:    \
    class internal {    \
    public: \
        internal() {    \
            init(); \
        }   \
            \
        type get() { \
            type ret = std::move(list_.front()); \
            list_.pop_front();  \
                                \
            if (list_.empty()) {    \
                init();     \
            }   \
                \
            return ret; \
        }   \
            \
        void init() {   \
            [this](auto&& ... args) {   \
                (list_.emplace_front(   \
                    std::forward<decltype(args)>(args)  \
                ), ...);   \
            }(__VA_ARGS__); \
        }   \
            \
    private:    \
        std::forward_list<type> list_;   \
    };  \
        \
    static internal data_;  \
};  \
    \
typename value_generator<type>::internal value_generator<type>::data_

__DEFINE_VALUE_GEN(int, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
__DEFINE_VALUE_GEN(std::unique_ptr<int>, new int(1), new int(2), new int(3), new int(4));
__DEFINE_VALUE_GEN(std::string, "yeah", "holy", "moly", "shit", "fuck");

#undef __DEFINE_VALUE_GEN

}   // namespace test_values::detail

template <class T>
T gen() {
    return detail::value_generator<T>::gen();
}

template <class T, template <typename> class Cont = std::vector>
Cont<T> gen(std::size_t n) {
    Cont<T> ret;
    while (n--) {
        ret.push_back( gen<T>() );
    }

    return ret;
}

}   // namespace test_values

#endif  // __test_values