#define LRU_CACHE_CHECK_INVARIANT

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

    // read random range in cache and let n the range's size,
    // then reading first n elements from the cache in reverse order
    // will yield equal result with reading from original range.

    // select range
    auto random_idx_first = dist(re);
    auto random_idx_last = dist(re);

    if (random_idx_first > random_idx_last) {
        std::swap(random_idx_first, random_idx_last);
    }

    // calculate n
    const auto range_length = random_idx_last - random_idx_first;

    auto it_first = std::next(cache.begin(), random_idx_first);
    const auto it_last = std::next(it_first, range_length);

    // read values in the range and store their addresses
    // Note: since LRU mechanism is implemented by std::list,
    // all nodes are guaranteed to be not invalidated,
    // which means the values' addresses are conserved.
    auto read_values = std::vector<
        typename std::remove_cvref_t<CacheT>::const_pointer
    >();

    for (; it_first != it_last; ++it_first) {
        read_values.push_back( &(*it_first) );
    }

    // test first n elements from cache in reverse order
    // with stored original range.
    auto src_first = read_values.begin();
    const auto src_last = std::next(src_first, range_length);

    auto read_first = cache.make_reverse_iterator(
        std::next(cache.begin(), range_length)
    );


    for (; src_first != src_last; ++src_first, ++read_first) {
        const auto& val = *read_first;
        EXPECT_EQ(*src_first, &val);
    }
}

template <class KeyT, class MappedT>
struct LRUTestInfo {
    using key_type = KeyT;
    using mapped_type = MappedT;
    using value_type = std::pair<key_type, mapped_type>;

    std::vector<value_type> values;
    std::size_t cacheline_size;
    std::size_t num_cacheline;
};

template <class TestInfoT>
class TLRUTestSuite : public ::testing::Test {
public:
    using info_type = TestInfoT;
    using value_type = typename TestInfoT::value_type;
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

    if constexpr ( std::is_lvalue_reference_v<TestInfoT> ) {
        std::ranges::for_each( test_info.values,
            [&cache](const auto& elem) { cache.insert( elem ); }
        );
    }
    else {
        std::ranges::for_each( std::move(test_info.values),
            [&cache](auto& elem) {
                cache.insert( std::move(elem) );
            }
        );
    }

    LRU_test_body(cache);
}

#define IsLRUBody() \
    IsLRU_impl( info_type{  \
        .values = test_values::gen<value_type>(14u),    \
        .cacheline_size = 2u,   \
        .num_cacheline = 0x10u  \
    } )

TEST_F(LRUTestSuiteBasic, IsLRU) {
    IsLRUBody();
}

TEST_F(LRUTestSuiteKeyNotDefaultConstructible, IsLRU) {
    IsLRUBody();
}

TEST_F(LRUTestSuiteValueNotDefaultConstructible, IsLRU) {
    IsLRUBody();
}

TEST_F(LRUTestSuiteKeyNotCopyable, IsLRU) {
    IsLRUBody();
}

TEST_F(LRUTestSuiteValueNotCopyable, IsLRU) {
    IsLRUBody();
}