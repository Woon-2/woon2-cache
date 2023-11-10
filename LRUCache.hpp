#ifndef __LRUCache
#define __LRUCache

#include <ranges>
#include <iterator>
#include <unordered_map>
#include <list>
#include <tuple>
#include <functional>
#include <memory>
#include <algorithm>
#include <cassert>
#include <cmath>
#include <memory_resource>
#include <type_traits>
#include <optional>

template <class Key, class T, class Hash = std::hash<Key>,
    class KeyEqual = std::equal_to<Key>,
    template <typename> class TAlloc = std::allocator
>
class LRUCache {
public:
    using key_type = Key;
    using mapped_type = T;
    using value_type = std::pair<const key_type, mapped_type>;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;
    using hasher = Hash;
    using key_equal = KeyEqual;
    using reference = value_type&;
    using const_reference = const value_type&;
    using allocator_type = TAlloc<value_type>;
    using pointer = typename std::allocator_traits<
        allocator_type>::pointer;
    using const_pointer = typename std::allocator_traits<
        allocator_type>::const_pointer;

private:
    using my_type = LRUCache<Key, T, Hash, KeyEqual, TAlloc>;
    using list_type = std::list<value_type, allocator_type>;
    using mapped_iterator
        = std::ranges::iterator_t<list_type>;
    using mapped_const_iterator
        = typename list_type::const_iterator;
    using map_value_type = std::pair<const key_type,
        mapped_iterator
    >;
    using map_allocator_type = TAlloc<map_value_type>;
    using map_type = std::unordered_map<
        key_type, mapped_iterator, hasher,
        key_equal, map_allocator_type
    >;

    class cache_result {
    public:
        cache_result() = default;

        cache_result(const std::optional<
            std::reference_wrapper<mapped_type>
        >& data) : data_(data) {}

        cache_result(std::optional<
            std::reference_wrapper<mapped_type>
        >&& data) : data_(std::move(data)) {}

        bool hit() const noexcept {
            return data_.has_value();
        }

        bool missed() const noexcept {
            return !hit();
        }

        mapped_type& value() {
            return data_.value();
        }

        const mapped_type& value() const {
            return data_.value();
        }

        mapped_type& operator*() {
            return value();
        }

        const mapped_type& operator*() const {
            return value();
        }

        mapped_type* operator->() {
            return &value();
        }

        const mapped_type* operator->() const {
            return &value();
        }

        operator mapped_type& () {
            return value();
        }

        operator const mapped_type& () {
            return value();
        }

        operator bool() const noexcept {
            return hit();
        }

        friend constexpr auto operator<=>(
            const cache_result& lhs, const cache_result& rhs
            ) {
            return lhs.data_ <=> rhs.data_;
        }

    private:
        std::optional<std::reference_wrapper<mapped_type>> data_;
    };

    class iterator {
    public:
        friend class my_type::const_iterator;

        using iterator_type = mapped_iterator;
        using difference_type = std::ptrdiff_t;
        using value_type = std::iter_value_t<iterator_type>;
        using reference = std::iter_reference_t<iterator_type>;
        using pointer = std::add_pointer_t<reference>;
        using iterator_category = std::bidirectional_iterator_tag;

        iterator() = default;

        iterator(LRUCache& base, mapped_iterator iter)
            : iter_(iter), bookmark_(), base_(&base) {}

        reference operator*() const {
            // guarantee the iterator returns the same reference
            // when it's read more than once.
            if (bookmark_.has_value()) {
                return *(bookmark_.value());
            }

            // since it's a read operation,
            // update cache via calling at.
            auto tmp = std::next(iter_);
            auto& key = iter_->first;
            volatile auto res = base_->at(key);
            bookmark_ = iter_;
            iter_ = std::prev(tmp);
            return *(bookmark_.value());
        }

        pointer operator->() const {
            return &(this->operator*());
        }

        iterator& operator++() {
            ++iter_;
            bookmark_ = {};
            return *this;
        }

        iterator operator++(int) {
            iterator tmp = *this;
            ++(*this);
            return tmp;
        }

        iterator& operator--() {
            --iter_;
            bookmark_ = {};
            return *this;
        }

        iterator operator--(int) {
            iterator tmp = *this;
            --(*this);
            return tmp;
        }

        friend bool operator==(const iterator& lhs,
            const iterator& rhs
            ) {
            return lhs.iter_ == rhs.iter_;
        }

        friend bool operator!=(const iterator& lhs,
            const iterator& rhs
            ) {
            return !(lhs == rhs);
        }


    private:
        static reference get_raw(const iterator& iter) {
            return *iter.iter_;
        }

        mutable iterator_type iter_;
        mutable std::optional<iterator_type> bookmark_;
        mutable LRUCache* base_;
    };

    class const_iterator {
    public:
        using const_iterator_type = mapped_const_iterator;
        using difference_type = std::ptrdiff_t;
        using value_type = std::iter_value_t<const_iterator_type>;
        using const_reference = std::iter_reference_t<const_iterator_type>;
        using const_pointer = std::add_pointer_t<const_reference>;
        using iterator_category = std::bidirectional_iterator_tag;

        const_iterator() = default;

        const_iterator(const LRUCache& base, mapped_const_iterator iter)
            : iter_(iter), bookmark_(), base_(&base) {}

        const_iterator(const iterator& iter)
            : iter_(iter.iter_), base_(iter.base_) {}

        const_reference operator*() const {
            // guarantee the iterator returns the same reference
            // when it's read more than once.
            if (bookmark_.has_value()) {
                return *(bookmark_.value());
            }

            // since it's a read operation,
            // update cache via calling at.
            auto tmp = std::next(iter_);
            auto& key = iter_->first;
            volatile auto res = base_->at(key);
            bookmark_ = iter_;
            iter_ = std::prev(tmp);
            return *(bookmark_.value());
        }

        const_pointer operator->() const {
            return &(this->operator*());
        }

        const_iterator& operator++() {
            ++iter_;
            return *this;
        }

        const_iterator operator++(int) {
            const_iterator tmp = *this;
            ++(*this);
            return tmp;
        }

        const_iterator& operator--() {
            --iter_;
            return *this;
        }

        const_iterator operator--(int) {
            const_iterator tmp = *this;
            --(*this);
            return tmp;
        }

        friend bool operator==(const const_iterator& lhs,
            const const_iterator& rhs
            ) {
            return lhs.iter_ == rhs.iter_;
        }

        friend bool operator!=(const const_iterator& lhs,
            const const_iterator& rhs
            ) {
            return !(lhs == rhs);
        }


    private:
        static const_reference get_raw(const const_iterator& iter) {
            return *iter.iter_;
        }

        mutable const_iterator_type iter_;
        mutable std::optional<const_iterator_type> bookmark_;
        const LRUCache* base_;
    };

public:
    static constexpr size_type default_cacheline_size = 0x04;
    static constexpr size_type default_num_cacheline = 0x0100;

    explicit LRUCache(
        size_type cacheline_size = default_cacheline_size,
        size_type num_cacheline = default_num_cacheline,
        const hasher& hash = hasher(),
        const key_equal& equal = key_equal(),
        const allocator_type& alloc = allocator_type()
    ) : LRU_list_(alloc),
        map_(num_cacheline, map_allocator_type()),
        cacheline_size_(cacheline_size),
        num_cacheline_(num_cacheline) {
        reserve_map();
    }

    LRUCache(size_type cacheline_size,
        size_type num_cacheline,
        const allocator_type& alloc
    ) : LRUCache(cacheline_size, num_cacheline,
        hasher(), key_equal(), alloc
    ) {

    }

    LRUCache(size_type cacheline_size,
        size_type num_cacheline,
        const hasher& hash,
        const allocator_type& alloc
    ) : LRUCache(cacheline_size, num_cacheline,
        hash, key_equal(), alloc
    ) {

    }

    explicit LRUCache(const allocator_type& alloc)
        : LRUCache(default_cacheline_size,
            default_num_cacheline, hasher(),
            key_equal(), alloc
        ) {

    }

    LRUCache(const LRUCache& other)
        : LRU_list_(other.LRU_list_),
        map_(),
        cacheline_size_(other.cacheline_size_),
        num_cacheline_(other.num_cacheline_) {
        reserve_map();
        construct_map();
    }

    LRUCache(const LRUCache& other, const allocator_type& alloc)
        : LRU_list_(other.LRU_list_, alloc),
        map_(),
        cacheline_size_(other.cacheline_size_),
        num_cacheline_(other.num_cacheline_) {
        reserve_map();
        construct_map();
    }

    LRUCache(const LRUCache& other, const map_allocator_type& map_alloc)
        : LRU_list_(other.LRU_list_),
        map_(map_alloc),
        cacheline_size_(other.cacheline_size_),
        num_cacheline_(other.num_cacheline_) {
        reserve_map();
        construct_map();
    }

    LRUCache(const LRUCache& other, 
        const allocator_type& alloc,
        const map_allocator_type& map_alloc
    )
        : LRU_list_(other.LRU_list_, alloc),
        map_(map_alloc),
        cacheline_size_(other.cacheline_size_),
        num_cacheline_(other.num_cacheline_) {
        reserve_map();
        construct_map();
    }

    LRUCache& operator=(const LRUCache& other) {
        LRUCache(other).swap(*this);
    }

    LRUCache(LRUCache&& other) noexcept
        : LRU_list_(std::move(other.LRU_list_)),
        map_(),
        cacheline_size_(std::move(other.cacheline_size_)),
        num_cacheline_(std::move(other.num_cacheline_)) {
        other.cacheline_size_ = size_type(0);
        other.num_cacheline_ = size_type(0);
        reserve_map();
        construct_map();
    }

    LRUCache(LRUCache&& other, const allocator_type& alloc) noexcept
        : LRU_list_(std::move(other.LRU_list_), alloc),
        map_(),
        cacheline_size_(std::move(other.cacheline_size_)),
        num_cacheline_(std::move(other.num_cacheline_)) {
        other.cacheline_size_ = size_type(0);
        other.num_cacheline_ = size_type(0);
        reserve_map();
        construct_map();
    }

    LRUCache(LRUCache&& other, const map_allocator_type& map_alloc) noexcept
        : LRU_list_(std::move(other.LRU_list_)),
        map_(map_alloc),
        cacheline_size_(std::move(other.cacheline_size_)),
        num_cacheline_(std::move(other.num_cacheline_)) {
        other.cacheline_size_ = size_type(0);
        other.num_cacheline_ = size_type(0);
        reserve_map();
        construct_map();
    }

    LRUCache(LRUCache&& other, const allocator_type& alloc, 
        const map_allocator_type& map_alloc
    ) noexcept
        : LRU_list_(std::move(other.LRU_list_), alloc),
        map_(map_alloc),
        cacheline_size_(std::move(other.cacheline_size_)),
        num_cacheline_(std::move(other.num_cacheline_)) {
        other.cacheline_size_ = size_type(0);
        other.num_cacheline_ = size_type(0);
        reserve_map();
        construct_map();
    }

    ~LRUCache() = default;

    LRUCache& operator=(LRUCache&& other) noexcept {
        LRUCache(std::move(other)).swap(*this);
    }

    iterator begin() {
        return iterator(*this, LRU_list_.begin());
    }

    iterator end() {
        return iterator(*this, LRU_list_.end());
    }

    const_iterator begin() const {
        return cbegin();
    }

    const_iterator end() const {
        return cend();
    }

    const_iterator cbegin() const {
        return const_iterator(*this, LRU_list_.begin());
    }

    const_iterator cend() const {
        return const_iterator(*this, LRU_list_.end());
    }

    [[maybe_unused]] cache_result at(const key_type& key) {
        return at_impl(key);
    }

    [[maybe_unused]] const cache_result at(
        const key_type& key
    ) const {
        return at_impl(key);
    }

    // xxxxxxxxxxxxxxxxxxx
    template <class KeyU>
    [[maybe_unused]] mapped_type& get(KeyU&& key) {
        return get( std::forward<KeyU>(key), mapped_type() );
    }

    template <class KeyU>
    [[maybe_unused]] const mapped_type& get(KeyU&& key) const {
        return const_cast<my_type*>(this)
            ->get(std::forward<KeyU>(key));
    }

    template <class KeyU, class MappedT>
    [[maybe_unused]] mapped_type& get( KeyU&& key,
        MappedT&& mapped_value
    ) {
        return get( std::forward<KeyU>(key), [](auto&& mv) { 
            return std::forward<decltype(mv)>(mv); 
        });
    }

    template <class KeyU, class MappedT>
    [[maybe_unused]] const mapped_type& get( KeyU&& key,
        MappedT&& mapped_value
    ) const {
        return const_cast<my_type*>(this)
            ->get( std::forward<KeyU>(key),
                std::forward<MappedT>(mapped_value)
            );
    }

    template <class KeyU, class Gen, class ... Args>
    [[maybe_unused]] mapped_type& get(
        KeyU&& key, Gen&& gen, Args&& ... args
    ) {
        if ( auto cache_res = at( std::forward<KeyU>(key) );
            cache_res.hit()
        ) {
            return cache_res.value();
        }

        do_insert( std::invoke(
            std::forward<Gen>(gen), std::forward<Args>(args)...
        ) );

        return LRU_list_.front().second;
    }

    template <class KeyU, class Gen, class ... Args>
    [[maybe_unused]] const mapped_type& get(
        KeyU&& key, Gen&& gen, Args&& ... args
    ) const {
        return const_cast<my_type*>(this)->get(
            std::forward<KeyU>(key), std::forward<Gen>(gen),
            std::forward<Args>(args)...
        );
    }

    [[maybe_unused]] cache_result operator[](const key_type& key) {
        return get(key);
    }

    [[maybe_unused]] cache_result operator[](key_type&& key) {
        return get(std::move(key));
    }

    [[maybe_unused]] const cache_result operator[](
        const key_type& key
    ) const {
        return const_cast<my_type*>(this)->operator[](key);
    }

    [[maybe_unused]] const cache_result operator[](
        key_type&& key
    ) const {
        return const_cast<my_type*>(this)
            ->operator[](std::move(key));
    }

    size_type size() const noexcept {
        return LRU_list_.size();
    }

    size_type capacity() const noexcept {
        return cacheline_size() * num_cacheline();
    }

    std::pair<mapped_iterator, bool> insert(
        const value_type& value
    ) {
        if (at(value.first).hit()) {
            return { LRU_list_.begin(), false };
        }

        do_insert(value);

        return { LRU_list_.begin(), true };
    }

    std::pair<mapped_iterator, bool> insert(
        value_type&& value
    ) {
        if (at(value.first).hit()) {
            return { LRU_list_.begin(), false };
        }

        do_insert(std::move(value));

        return { LRU_list_.begin(), true };
    }

    template <class MappedT>
    std::pair<mapped_iterator, bool> insert_or_assign(
        const key_type& key, MappedT&& mapped_value
    ) {
        return insert_or_assign_impl( key,
            std::forward<MappedT>(mapped_value)
        );
    }

    template <class MappedT>
    std::pair<mapped_iterator, bool> insert_or_assign(
        key_type&& key, MappedT&& mapped_value
    ) {
        return insert_or_assign_impl( std::move(key),
            std::forward<MappedT>(mapped_value)
        );
    }

    template <class KeyU, class ... Args>
    std::pair<mapped_iterator, bool> try_emplace(
        KeyU&& key, Args&& ... args
    ) {
        if (at(key).hit()) {
            return { LRU_list_.begin(), false };
        }

        do_emplace(std::forward<KeyU>(key),
            std::forward<Args>(args)...
        );

        return { LRU_list_.begin(), true };
    }

    template <class ... Args>
    std::pair<mapped_iterator, bool> emplace(Args&& ... args) {
        auto tmp = value_type(std::forward<Args>(args)...);
        auto& key = tmp.first;

        if (at(key).hit()) {
            return { LRU_list_.begin(), false };
        }

        do_insert(std::move(tmp));

        return { LRU_list_.begin(), true };
    }

    size_type cacheline_size() const noexcept {
        return cacheline_size_;
    }

    size_type num_cacheline() const noexcept {
        return num_cacheline_;
    }

    void adjust() {
        if (size() > capacity()) {
            auto num_overflow_cacheline
                = std::max<size_type>(
                    static_cast<size_type>(std::ceil(
                        size() / static_cast<double>(cacheline_size())
                    )) - num_cacheline(),
                    0
                );

            // for poor support of list's erasing range in C++20,
            // use subrange and its iterators alternatively.
            auto invalidate_range = std::ranges::subrange{
                std::prev(LRU_list_.end(), cacheline_size()),
                LRU_list_.end()
            };

            std::ranges::for_each(invalidate_range,
                [this](const auto& elem) {
                    map_.erase(elem.first);
                }
            );

            LRU_list_.erase(invalidate_range.begin(),
                invalidate_range.end()
            );

            check_invariant();
        }
    }

    void clear() {
        LRU_list_.clear();
        map_.clear();
        check_invariant();
    }

    iterator find(const key_type& key) {
        if (!map_.contains(key)) {
            return end();
        }

        return iterator(*this, map_.at(key));
    }

    const_iterator find(const key_type& key) const {
        if (!map_.contains(key)) {
            return cend();
        }

        return const_iterator(*this, map_.at(key));
    }

    allocator_type get_allocator() const noexcept {
        return allocator_type();
    }

    void set_cacheline_size(size_type n) {
        cacheline_size_ = n;
        adjust();
    }

    void set_num_cacheline(size_type n) {
        num_cacheline_ = n;
        adjust();
    }

    bool contains(const key_type& key) const {
        return at(key).hit();
    }

    bool contains(key_type&& key) const {
        return at( std::move(key) ).hit();
    }

    void swap(my_type& other) noexcept(
        noexcept(
            std::allocator_traits<
                map_allocator_type
            >::is_always_equal::value
            && std::is_nothrow_swappable<Hash>::value
            && std::is_nothrow_swappable<key_equal>::value
            && std::allocator_traits<
                allocator_type
            >::is_always_equal::value
        )
    ){
        std::swap(LRU_list_, other.LRU_list_);
        std::swap(map_, other.map_);
        std::swap(cacheline_size_, other.cacheline_size_);
        std::swap(num_cacheline_, other.num_cacheline_);
    }

    friend auto operator<=>(const LRUCache& lhs, const LRUCache& rhs) {
        if (auto size_compare_result = lhs.size() <=> rhs.size();
            size_compare_result != 0
        ) {
            return size_compare_result;
        }

        auto lbegin = lhs.LRU_list.begin();
        auto lend = lhs.LRU_list.end();
        auto rbegin = rhs.LRU_list.begin();

        for (; lbegin != lend; ++lbegin, ++rbegin) {
            if (auto elem_compare_result = *lhs <=> *rhs;
                elem_compare_result != 0
            ) {
                return elem_compare_result;
            }
        }
    }

private:
    std::optional<std::reference_wrapper<mapped_type>>
        at_impl(const key_type& key) const {
        if (!map_.contains(key)) {
            return {};
        }

        LRU_list_.splice(
            LRU_list_.begin(), LRU_list_, map_.at(key)
        );

        check_invariant();

        return LRU_list_.front().second;
    }

    std::optional<std::reference_wrapper<mapped_type>>
        get_raw(const key_type& key) const {
        if (!map_.contains(key)) {
            return {};
        }

        return map_.at(key);
    }

#ifdef LRU_CACHE_CHECK_INVARIANT
    void check_invariant() const {
        assert(LRU_list_.size() == map_.size());

        for (auto it = LRU_list_.begin();
            it != LRU_list_.end(); ++it
            ) {
            assert(map_.contains(it->first));
            assert(map_.at(it->first) == it);
        }
    }
#else
    void check_invariant() const noexcept {}
#endif

    template <class ... Args>
    void do_emplace(Args&& ... args) {
        LRU_list_.emplace_front(std::forward<Args>(args)...);
        map_.emplace(LRU_list_.front().first, LRU_list_.begin());

        adjust();
        check_invariant();
    }

    template <class ValT>
    void do_insert(ValT&& val) {
        LRU_list_.push_front(std::forward<ValT>(val));
        map_.emplace(LRU_list_.front().first, LRU_list_.begin());

        adjust();
        check_invariant();
    }

    template <class KeyU, class MappedT>
    std::pair<mapped_iterator, bool> insert_or_assign_impl(
        KeyU&& key, MappedT&& mapped_value
    ) {
        if (auto cacheRes = at(key); cacheRes.hit()) {
            *cacheRes = std::forward<MappedT>(mapped_value);
            return { LRU_list_.begin(), false };
        }

        do_emplace( std::forward<KeyU>(key),
            std::forward<MappedT>(mapped_value)
        );

        return { LRU_list_.begin(), true };
    }

    void reserve_map() {
        map_.reserve(capacity());
    }

    void construct_map() {
        assert(map_.empty());

        for (auto it = LRU_list_.begin(); it != LRU_list_.end(); ++it) {
            map_.try_emplace( it->first, it );
        }

        check_invariant();
    }

    mutable list_type LRU_list_;
    map_type map_;
    size_type cacheline_size_;
    size_type num_cacheline_;
};

#endif  // __LRUCache