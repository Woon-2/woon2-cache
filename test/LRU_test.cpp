#include "gtest/gtest.h"
#include "LRUCache.hpp"
#include "random_object.hpp"

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

    auto copied = cache;

    auto random_idx_first = dist(re);
    auto random_idx_last = dist(re);

    if (random_idx_first > random_idx_last) {
        std::swap(random_idx_first, random_idx_last);
    }

    const auto range_length = random_idx_last - random_idx_first;

    auto it_first = std::next(copied.begin(), random_idx_first);
    const auto it_last = std::next(it_first, range_length);

    for (; it_first != it_last; ++it_first) {
        volatile const auto& tmp = *it_first;
    }

    std::ranges::for_each_n(copied.begin(), range_length,
        [](volatile const auto& elem) {}
    );

    auto src_first = std::next(
        std::forward<CacheT>(cache).begin(),
        random_idx_first
    );
    const auto src_last = std::next(src_first, range_length);

    auto read_first = copied.begin();

    for (; src_first != src_last; ++src_first, ++read_first) {
        EXPECT_EQ(*src_first, *read_first);
    }
}

template <class KeyT, class MappedT>
struct LRUTestInfo {
    using key_type = KeyT;
    using mapped_type = MappedT;
    using value_type = std::pair<const key_type, mapped_type>;

    std::size_t cacheline_size;
    std::size_t num_cacheline;
    std::vector<value_type> values;
};

template <class TestInfoT>
class TLRUTestSuite : public ::testing::TestWithParam<TestInfoT> {
public:

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
    
using TestInfoKeyNotLessComparable
    = LRUTestInfo<std::type_info, std::string>;

using LRUTestSuiteBasic = TLRUTestSuite<TestInfoBasic>;

template <class TestInfoT>
void IsLRU_impl(const TestInfoT& test_info) {
    using key_type = typename TestInfoT::key_type;
    using mapped_type = typename TestInfoT::mapped_type;

    auto cache = LRUCache<key_type, mapped_type>(
        test_info.cacheline_size,
        test_info.num_cacheline
    );

    std::ranges::for_each(test_info.values, [&cache](auto&& elem) {
        cache.insert( std::forward<decltype(elem)>(elem) );
    });

    // std::ranges::move(
    //     test_info.values(),
    //     std::front_inserter(cache)
    // );

    LRU_test_body(cache);
}

TEST_P(LRUTestSuiteBasic, IsLRU) {
    const auto& test_info = GetParam();
    IsLRU_impl(test_info);
}

INSTANTIATE_TEST_SUITE_P(MeenyMinyMoe,
    LRUTestSuiteBasic,
    testing::Values<TestInfoBasic>(
        {
            .cacheline_size = 4u,
            .num_cacheline = 0x10u,
            .values = {
                {1, "yeah"},
                {2, "fuck"},
                {3, "suck"},
                {4, "my"},
                {5, "dick"},
                {6, "girl"}
            }
        }
    )
);

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