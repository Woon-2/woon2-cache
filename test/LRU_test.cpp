#include "gtest/gtest.h"
#include "LRUCache.hpp"
#include "random_object.hpp"
#include "test_values.hpp"

#include <ranges>
#include <iterator>
#include <string>
#include <vector>
#include <typeinfo>
#include <typeindex>

template <class CacheT>
void LRU_test_body(CacheT&& cache) {
    auto dist = std::uniform_int_distribution<std::size_t>(
        0, cache.size()
    );

    // read random range in copied cache and let n the range's size,
    // then reading first n elements from the copied cache in reverse order
    // will yield equal result with reading from original range.

    // make copy
    auto copied = cache;

    // select range
    auto random_idx_first = dist(re);
    auto random_idx_last = dist(re);

    if (random_idx_first > random_idx_last) {
        std::swap(random_idx_first, random_idx_last);
    }

    // calculate n
    const auto range_length = random_idx_last - random_idx_first;

    auto it_first = std::next(copied.begin(), random_idx_first);
    const auto it_last = std::next(it_first, range_length);

    // actually read values in the range
    for (; it_first != it_last; ++it_first) {
        volatile const auto& tmp = *it_first;
    }

    // test first n elements from copied cache in reverse order
    // with the original range from original cache.
    auto src_first = std::next(
        std::forward<CacheT>(cache).begin(),
        random_idx_first
    );
    const auto src_last = std::next(src_first, range_length);

    auto read_first = copied.make_reverse_iterator(
        std::next(copied.begin(), range_length)
    );

    for (; src_first != src_last; ++src_first, ++read_first) {
        EXPECT_EQ(*src_first, *read_first);
    }
}

template <class KeyT, class MappedT>
struct LRUTestInfo {
    using key_type = KeyT;
    using mapped_type = MappedT;
    using value_type = std::pair<const key_type, mapped_type>;

    // LRUTestInfo() = default;

    // LRUTestInfo(std::size_t cacheline_size,
    //     std::size_t num_cacheline,
    //     std::vector<value_type> values
    // ) : values(values), cacheline_size(cacheline_size),
    //     num_cacheline(num_cacheline) {}

    // ~LRUTestInfo() = default;

    // LRUTestInfo(const LRUTestInfo& other) noexcept
    //     : values(std::move( const_cast<LRUTestInfo&>(other).values )),
    //     cacheline_size(other.cacheline_size),
    //     num_cacheline(other.num_cacheline) {}

    // LRUTestInfo& operator=(const LRUTestInfo& other) noexcept {
    //     LRUTestInfo(other).swap(*this);
    // }

    // void swap(LRUTestInfo& other) noexcept {
    //     std::swap(values, other.values);
    //     std::swap(cacheline_size, other.cacheline_size);
    //     std::swap(num_cacheline, other.num_cacheline);
    // }

    std::vector<value_type> values;
    std::size_t cacheline_size;
    std::size_t num_cacheline;
};

template <class TestInfoT>
class TLRUTestSuite : public ::testing::TestWithParam<TestInfoT> {
public:
    using info_type = TestInfoT;
private:
};

using TestInfoBasic = LRUTestInfo<int, std::string>;

using TestInfoKeyNotDefaultConstructible
    = LRUTestInfo<std::type_index, std::string>;

using TestInfoValueNotDefaultConstructible
    = LRUTestInfo<int, std::type_index>;

using TestInfoKeyNotCopyable
    = LRUTestInfo<std::unique_ptr<int>, std::string>;

using TestInfoValueNotCopyable
    = LRUTestInfo<int, std::unique_ptr<std::string>>;

using LRUTestSuiteBasic = TLRUTestSuite<TestInfoBasic>;

using LRUTestSuiteKeyNotDefaultConstructible
    = TLRUTestSuite<TestInfoKeyNotDefaultConstructible>;

using LRUTestSuiteValueNotDefaultConstructible
    = TLRUTestSuite<TestInfoValueNotDefaultConstructible>;

using LRUTestSuiteKeyNotCopyable
    = TLRUTestSuite<TestInfoKeyNotCopyable>;

using LRUTestSuiteValueNotCopyable
    = TLRUTestSuite<TestInfoValueNotCopyable>;

template <class TestInfoT>
void IsLRU_impl(TestInfoT&& test_info) {
    using key_type = typename std::remove_cvref_t<TestInfoT>::key_type;
    using mapped_type = typename std::remove_cvref_t<TestInfoT>::mapped_type;

    auto cache = LRUCache<key_type, mapped_type>(
        test_info.cacheline_size,
        test_info.num_cacheline
    );

    if constexpr ( not std::copyable< typename std::remove_cvref_t<TestInfoT>::key_type > ) {
        // assume key_type is an unique pointer
        std::ranges::for_each( std::forward<TestInfoT>(test_info).values,
            [&cache](const auto& elem) {
                cache.kggenerate(
                    [&elem]() {
                        auto val = *(elem.first);
                        return std::make_unique<decltype(val)>(val);
                    },
                    [&elem]() {
                        return elem.second;
                    }
                );
            }
        );
    }
    else {
        std::ranges::for_each( std::forward<TestInfoT>(test_info).values,
            [&cache](auto&& elem) {
                cache.insert( std::forward<decltype(elem)>(elem) );
            }
        );
    }

    LRU_test_body(cache);
}

#define IsLRUBody() \
    const auto& test_info = GetParam();  \
    IsLRU_impl(test_info)

TEST_P(LRUTestSuiteBasic, IsLRU) {
    IsLRUBody();
}

// TEST_P(LRUTestSuiteKeyNotDefaultConstructible, IsLRU) {
//     IsLRUBody();
// }

// TEST_P(LRUTestSuiteValueNotDefaultConstructible, IsLRU) {
//     IsLRUBody();
// }

// TEST_P(LRUTestSuiteKeyNotCopyable, IsLRU) {
//     IsLRUBody();
// }

// TEST_P(LRUTestSuiteValueNotCopyable, IsLRU) {
//     IsLRUBody();
// }

INSTANTIATE_TEST_SUITE_P(MeenyMinyMoe,
    LRUTestSuiteBasic,
    testing::Values<LRUTestSuiteBasic::info_type>(
        {
            .values = test_values::gen<
                LRUTestSuiteBasic::info_type::value_type
            >(10),
            .cacheline_size = 4u,
            .num_cacheline = 0x10u
        }
    )
);

// INSTANTIATE_TEST_SUITE_P(MeenyMinyMoexx,
//     LRUTestSuiteKeyNotCopyable,
//     testing::ValuesIn(
//         {
//             .values = {
//                 {test_values::gen<std::unique_ptr<int>>(), "yeah"},
//                 {test_values::gen<std::unique_ptr<int>>(), "fuck"},
//                 {test_values::gen<std::unique_ptr<int>>(), "suck"},
//                 {test_values::gen<std::unique_ptr<int>>(), "my"},
//                 {test_values::gen<std::unique_ptr<int>>(), "dick"},
//                 {test_values::gen<std::unique_ptr<int>>(), "girl"}
//             },
//             .cacheline_size = 4u,
//             .num_cacheline = 0x10u
//         }
//     )
// );

TEST(LRUTestSuite, LRUTest) {
    constexpr auto cnt = 0x100;
    auto dist_key = std::uniform_int_distribution<>(0, 1000);
    auto dist_char = std::uniform_int_distribution<>('a', 'z');
    auto dist_str_size = std::uniform_int_distribution<std::size_t>(
        1, 64
    );
    auto dist_iter = std::uniform_int_distribution<>(40, 100);
    auto dist_cacheline_size = std::uniform_int_distribution<std::size_t>(
        1, 20
    );
    auto dist_num_cacheline = std::uniform_int_distribution<std::size_t>(
        1, 0x20
    );
    auto dist_cache_size = std::uniform_int_distribution<std::size_t>(
        0, 0x800
    );

    for (int i = cnt; i--;) {
        auto cache = LRUCache<int, std::string>(
            dist_cacheline_size(re),
            dist_num_cacheline(re)
        );

        for (int j = dist_cache_size(re); j--;) {
            auto key = dist_key(re);
            auto value = std::string();
            for (int k = dist_str_size(re); k--;) {
                value += static_cast<char>( dist_char(re) );
            }

            cache.try_emplace(std::move(key), std::move(value));
        }

        for (int l = dist_iter(re); l--;) {
            LRU_test_body(cache);
        }
    }
}