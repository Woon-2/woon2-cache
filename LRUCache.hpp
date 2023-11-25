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
#include <type_traits>
#include <optional>
#include <concepts>

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
    struct hasher_impl {
        constexpr std::size_t operator()(
            const std::reference_wrapper<const key_type>& key_wrapper
        ) const {
            return hasher{}( key_wrapper.get() );
        }
    };

    using my_type = LRUCache<Key, T, Hash, KeyEqual, TAlloc>;
    using list_type = std::list<value_type, allocator_type>;
    using mapped_iterator
        = std::ranges::iterator_t<list_type>;
    using mapped_const_iterator
        = typename list_type::const_iterator;
    using map_value_type = std::pair<
        const std::reference_wrapper<const key_type>,
        mapped_iterator
    >;
    using map_allocator_type = TAlloc<map_value_type>;
    using map_type = std::unordered_map<
        std::reference_wrapper<const key_type>, mapped_iterator,
        hasher_impl, key_equal, map_allocator_type
    >;
    using ref_mapped_proxy = std::reference_wrapper<mapped_type>;


    class cache_result {
    public:
        cache_result() = default;

        // construct from std::optional<ref_mapped_proxy> to support implicit conversion
        // of return value of at_impl (which is same as std::optional<ref_mapped_proxy>)
        // to cache_result.
        cache_result(const std::optional<ref_mapped_proxy>& data)
            : data_(data) {}

        cache_result(std::optional<ref_mapped_proxy>&& data)
            : data_(std::move(data)) {}

        bool hit() const noexcept {
            return data_.has_value();
        }

        bool missed() const noexcept {
            return !hit();
        }

        mapped_type& value() {
            return data_.value().get();
        }

        const mapped_type& value() const {
            return data_.value().get();
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

        operator mapped_type&() {
            return value();
        }

        operator const mapped_type&() const {
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
        std::optional<ref_mapped_proxy> data_;
    };

    template <std::bidirectional_iterator SrcIt>
    class iterator_base {
    public:
        using source_iterator_type = SrcIt;
        using difference_type = std::iter_difference_t<source_iterator_type>;
        using value_type = std::iter_value_t<source_iterator_type>;
        using reference = std::iter_reference_t<source_iterator_type>;
        using pointer = typename std::iterator_traits<source_iterator_type>
            ::pointer;
        using iterator_category = std::bidirectional_iterator_tag;

        iterator_base() = default;

        iterator_base(LRUCache& base, source_iterator_type iter)
            : src_it_(iter), captured_(), base_(&base) {}

        reference operator*() const {
            // guarantee the iterator returns the same reference
            // when it's read more than once.
            if (captured_.has_value()) {
                return *(captured_.value());
            }

            // since it's a read operation,
            // update cache via calling at.
            auto tmp = std::next(src_it_);
            auto& key = src_it_->first;
            base_->at(key);
            captured_ = src_it_;
            src_it_ = std::prev(tmp);
            return *(captured_.value());
        }

        pointer operator->() const {
            return std::pointer_traits<pointer>::pointer_to(**this);
        }

        iterator_base& operator++() {
            ++src_it_;
            captured_.reset();
            return *this;
        }

        iterator_base operator++(int) {
            iterator_base tmp = *this;
            ++(*this);
            return tmp;
        }

        iterator_base& operator--() {
            // if it had been read just before call of this function,
            // the previous iterator's relative position has been advanced.
            // e.g. read "3": [1, 2, 3, 4, 5] -> [3, 1, 2, 4, 5]
            // so invalidate the bookmark and don't update the iterator in this case.
            if (captured_.has_value()) {
                captured_.reset();
                return *this;
            }
            --src_it_;
            return *this;
        }

        iterator_base operator--(int) {
            iterator tmp = *this;
            --(*this);
            return tmp;
        }

        source_iterator_type base() const noexcept {
            return src_it_;
        }

        friend bool operator==(const iterator_base& lhs,
            const iterator_base& rhs
        ) {
            return lhs.src_it_ == rhs.src_it_;
        }

        friend bool operator!=(const iterator_base& lhs,
            const iterator_base& rhs
        ) {
            return !(lhs == rhs);
        }

    protected:
        iterator_base(LRUCache& base, source_iterator_type iter,
            source_iterator_type captured
        )
            : src_it_(iter), captured_(captured), base_(&base) {}

        std::optional<source_iterator_type> captured() const noexcept {
            return captured_;
        }

        void set_captured( source_iterator_type it ) const {
            captured_ = it;
        }

    private:
        mutable source_iterator_type src_it_;
        mutable std::optional<source_iterator_type> captured_;
        mutable LRUCache* base_;
    };

    template <class SrcIt>
    class reverse_iterator_base {
    public:
        using iterator_type = SrcIt;
        using iterator_category = typename std::iterator_traits<iterator_type>
            ::iterator_category;
        using value_type = std::iter_value_t<iterator_type>;
        using difference_type = std::iter_difference_t<iterator_type>;
        using pointer = typename std::iterator_traits<iterator_type>
            ::pointer;
        using reference = std::iter_reference_t<iterator_type>;

        reverse_iterator_base() = default;

        reverse_iterator_base(iterator_type base)
            : base_(base) {}

        iterator_type base() const {
            return base_;
        }

        reference operator*() const {
            if ( auto captured = base_.captured(); captured.has_value() ) {
                return *(captured.value());
            }

            auto tmp = base_;
            auto& ret = *(--tmp);
            assert( tmp.captured().has_value() );
            base_.set_captured( tmp.captured().value() );

            return ret;
        }

        pointer operator->() const
            requires std::is_pointer_v<iterator_type>
                || requires (const iterator_type i) { i.operator->(); } {
            auto tmp = base_;
            return (--tmp).operator->();
        }

        reverse_iterator_base& operator++() {
            --base_;
            return *this;
        }

        reverse_iterator_base operator++(int) {
            auto ret = base_;
            --(*this);
            return ret;
        }

        reverse_iterator_base& operator--() {
            ++base_;
            return *this;
        }
        
        reverse_iterator_base operator--(int) {
            auto ret = base_;
            ++(*this);
            return ret;
        }

        friend bool operator==(const reverse_iterator_base& lhs,
            const reverse_iterator_base& rhs
        ) {
            return lhs.base_ == rhs.base_;
        }

        friend bool operator!=(const reverse_iterator_base& lhs,
            const reverse_iterator_base& rhs
        ) {
            return !(lhs == rhs);
        }

    private:
        void swap(reverse_iterator_base& other) noexcept (
            noexcept( std::is_nothrow_swappable<iterator>::value )
        ) {
            std::swap(base_, other.base_);
        }

        iterator_type base_;
    };

public:
    class iterator : public iterator_base<mapped_iterator> {
    public:
        friend class LRUCache::const_iterator;
        template <class>
        friend class my_type::reverse_iterator_base;

        using parent = iterator_base<mapped_iterator>;

        using difference_type = typename parent::difference_type;
        using value_type = typename parent::value_type;
        using reference = typename parent::reference;
        using pointer = typename parent::pointer;
        using iterator_category = typename parent::iterator_category;

        iterator() = default;

        iterator(LRUCache& base, mapped_iterator iter)
            : iterator_base<mapped_iterator>(base, iter) {}
    };

    class const_iterator : public iterator_base<mapped_const_iterator> {
    public:
        template <class>
        friend class my_type::reverse_iterator_base;

        using parent = iterator_base<mapped_const_iterator>;

        using difference_type = typename parent::difference_type;
        using value_type = typename parent::value_type;
        using reference = typename parent::reference;
        using pointer = typename parent::pointer;
        using iterator_category = typename parent::iterator_category;

        const_iterator() = default;

        const_iterator(LRUCache& base, mapped_const_iterator iter)
            : iterator_base<mapped_const_iterator>(base, iter) {}

        const_iterator(iterator it)
            : iterator_base<mapped_const_iterator>(it.base_, it.src_it_, it.captured_) {}
    };

    class reverse_iterator : public reverse_iterator_base<iterator> {
    public:
        using parent = reverse_iterator_base<iterator>;

        using iterator_type = typename parent::iterator_type;
        using difference_type = typename parent::difference_type;
        using value_type = typename parent::value_type;
        using reference = typename parent::reference;
        using pointer = typename parent::pointer;
        using iterator_category = typename parent::iterator_category;

        reverse_iterator() = default;

        reverse_iterator(iterator_type base)
            : reverse_iterator_base<iterator>(base) {}

        template <class OtherIt>
            requires requires (const OtherIt i) {
                    {i.base()} -> std::convertible_to<iterator_type>;
                } 
        reverse_iterator(const OtherIt& other_it)
            : reverse_iterator_base<iterator>( other_it.base() ) {}

        template <class OtherIt>
            requires requires (const OtherIt i) {
                    {i.base()} -> std::convertible_to<iterator_type>;
                } 
        reverse_iterator& operator=(const OtherIt& other_it) {
            static_cast<reverse_iterator>(other_it).swap(*this);
        }
    };

    class const_reverse_iterator : public reverse_iterator_base<const_iterator> {
    public:
        using parent = reverse_iterator_base<const_reverse_iterator>;

        using iterator_type = typename parent::iterator_type;
        using difference_type = typename parent::difference_type;
        using value_type = typename parent::value_type;
        using reference = typename parent::reference;
        using pointer = typename parent::pointer;
        using iterator_category = typename parent::iterator_category;

        const_reverse_iterator() = default;

        const_reverse_iterator(iterator_type base)
            : reverse_iterator_base<const_iterator>(base) {}

        template <class OtherIt>
            requires requires (const OtherIt i) {
                    {i.base()} -> std::convertible_to<iterator_type>;
                } 
        const_reverse_iterator(const OtherIt& other_it)
            : reverse_iterator_base<const_iterator>( other_it.base() ) {}

        template <class OtherIt>
            requires requires (const OtherIt i) {
                    {i.base()} -> std::convertible_to<iterator_type>;
                } 
        const_reverse_iterator& operator=(const OtherIt& other_it) {
            static_cast<const_reverse_iterator>(other_it).swap(*this);
        }
    };

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
        return const_iterator( const_cast<my_type&>(*this), LRU_list_.cbegin() );
    }

    const_iterator cend() const {
        return const_iterator( const_cast<my_type&>(*this), LRU_list_.cend() );
    }

    reverse_iterator rbegin() {
        return reverse_iterator(begin());
    }

    reverse_iterator rend() {
        return reverse_iterator(end());
    }

    const_reverse_iterator rbegin() const {
        return const_reverse_iterator(cbegin());
    }

    const_reverse_iterator rend() const {
        return const_reverse_iterator(cend());
    }

    static reverse_iterator make_reverse_iterator(iterator it) {
        return reverse_iterator(it);
    }

    static const_reverse_iterator make_reverse_iterator(const_iterator it) {
        return const_reverse_iterator(it);
    }

    [[maybe_unused]] cache_result at(const key_type& key) {
        return at_impl(key);
    }

    [[maybe_unused]] const cache_result at(const key_type& key) const {
        return const_cast<my_type*>(this)->at(key);
    }

    template <class KeyU>
        requires std::equality_comparable_with<const KeyU&, const key_type&>
    [[maybe_unused]] cache_result at(const KeyU& key) {
        return at_impl( std::forward<KeyU>(key) );
    }

    template <class KeyU>
        requires std::equality_comparable_with<const KeyU&, const key_type&>
    [[maybe_unused]] const cache_result at(const KeyU& key) const {
        return const_cast<my_type*>(this)->at( std::forward<KeyU>(key) );
    }

    template <class KeyU>
        requires std::equality_comparable_with<const KeyU&, const key_type&>
            && std::constructible_from<key_type, KeyU>
            && std::is_default_constructible_v<mapped_type>
    [[maybe_unused]] mapped_type& get(KeyU&& key) {
        return get( std::forward<KeyU>(key), mapped_type() );
    }

    template <class KeyU>
        requires std::equality_comparable_with<const KeyU&, const key_type&>
            && std::constructible_from<key_type, KeyU>
            && std::is_default_constructible_v<mapped_type>
    [[maybe_unused]] const mapped_type& get(KeyU&& key) const {
        return const_cast<my_type*>(this)->get( std::forward<KeyU>(key) );
    }

    template <class KeyU, class MappedT>
        requires std::equality_comparable_with<const KeyU&, const key_type&>
            && std::constructible_from<key_type, KeyU>
            && std::constructible_from<mapped_type, MappedT>
    [[maybe_unused]] mapped_type& get( KeyU&& key, MappedT&& newVal ) {
        try_emplace( std::forward<KeyU>(key), std::forward<MappedT>(newVal) );
        // `LRU_list_.front().second`'s performance
        // is better than `this->begin()->second`,
        // (since the iterator has many members even involving the list's iterator,
        // and has to setup more things than LRU_list_'s.)
        // so use `LRU_list_.front().second`.
        return LRU_list_.front().second;
    }

    template <class KeyU, class MappedT>
        requires std::equality_comparable_with<const KeyU&, const key_type&>
            && std::constructible_from<key_type, KeyU>
            && std::constructible_from<mapped_type, MappedT>
    [[maybe_unused]] const mapped_type& get( KeyU&& key,
        MappedT&& newVal
    ) const {
        return const_cast<my_type*>(this)->get(
            std::forward<KeyU>(key), std::forward<MappedT>(newVal)
        );
    }

    template <class KeyU, class ValGen, class ... Args>
        requires std::equality_comparable_with<const KeyU&, const key_type&>
            && std::constructible_from<key_type, KeyU>
            && std::invocable<ValGen, Args...>
            && (!std::same_as< std::remove_cvref_t<ValGen>, mapped_type >)
            && std::constructible_from<
                mapped_type, std::invoke_result_t<ValGen, Args...>
            >
    [[maybe_unused]] mapped_type& get( KeyU&& key,
        ValGen&& gen, Args&& ... args
    ) {
        generate( std::forward<KeyU>(key), std::forward<ValGen>(gen),
            std::forward<Args>(args)...
        );
        return LRU_list_.front().second;
    }

    template <class KeyU, class ValGen, class ... Args>
        requires std::equality_comparable_with<const KeyU&, const key_type&>
            && std::constructible_from<key_type, KeyU>
            && std::invocable<ValGen, Args...>
            && (!std::same_as< std::remove_cvref_t<ValGen>, mapped_type >)
            && std::constructible_from<
                mapped_type, std::invoke_result_t<ValGen, Args...>
            >
    [[maybe_unused]] const mapped_type& get( KeyU&& key,
        ValGen&& gen, Args&& ... args
    ) const {
        return const_cast<my_type*>(this)->get(
            std::forward<KeyU>(key),
            std::forward<ValGen>(gen),
            std::forward<Args>(args)...
        );
    }

    template <class KeyGen, class ValGen, class ... Args>
        requires std::invocable<KeyGen>
            && (!std::same_as< std::remove_cvref_t<KeyGen>, key_type >)
            && std::constructible_from< key_type, std::invoke_result_t<KeyGen> >
            && std::invocable<ValGen, Args...>
            && (!std::same_as< std::remove_cvref_t<ValGen>, mapped_type >)
            && std::constructible_from<
                mapped_type, std::invoke_result_t<ValGen, Args...>
            >
    [[maybe_unused]] mapped_type& get(KeyGen&& key_gen,
        ValGen&& val_gen, Args&& ... args
    ) {
        generate(std::forward<KeyGen>(key_gen), std::forward<ValGen>(val_gen),
            std::forward<Args>(args)...
        );
        return LRU_list_.front().second;
    }

    template <class KeyGen, class ValGen, class ... Args>
        requires std::invocable<KeyGen>
            && (!std::same_as< std::remove_cvref_t<KeyGen>, key_type >)
            && std::constructible_from< key_type, std::invoke_result_t<KeyGen> >
            && std::invocable<ValGen, Args...>
            && (!std::same_as< std::remove_cvref_t<ValGen>, mapped_type >)
            && std::constructible_from<
                mapped_type, std::invoke_result_t<ValGen, Args...>
            >
    [[maybe_unused]] const mapped_type& get(KeyGen&& key_gen,
        ValGen&& val_gen, Args&& ... args
    ) const {
        return const_cast<my_type*>(this)->get(
            std::forward<KeyGen>(key_gen),
            std::forward<ValGen>(val_gen),
            std::forward<Args>(args)...
        );
    }

    template <class KeyU, class ValGen, class ... Args>
        requires std::equality_comparable_with<const KeyU&, const key_type&>
            && std::constructible_from<key_type, KeyU>
            && std::invocable<ValGen, Args...>
            && (!std::same_as< std::remove_cvref_t<ValGen>, mapped_type >)
            && std::constructible_from<
                mapped_type, std::invoke_result_t<ValGen, Args...>
            >
    [[maybe_unused]] bool generate( KeyU&& key,
        ValGen&& gen, Args&& ... args
    ) {
        if ( auto cache_res = at(key); cache_res.hit() ) {
            return false;
        }

        do_generate(std::forward<KeyU>(key),
            std::forward<ValGen>(gen),
            std::forward<Args>(args)...
        );

        return true;
    }

    template <class KeyGen, class ValGen, class ... Args>
        requires std::invocable<KeyGen>
            && (!std::same_as< std::remove_cvref_t<KeyGen>, key_type >)
            && std::constructible_from< key_type, std::invoke_result_t<KeyGen> >
            && std::invocable<ValGen, Args...>
            && (!std::same_as< std::remove_cvref_t<ValGen>, mapped_type >)
            && std::constructible_from<
                mapped_type, std::invoke_result_t<ValGen, Args...>
            >
    [[maybe_unused]] bool generate(KeyGen&& key_gen,
        ValGen&& val_gen, Args&& ... args
    ) {
        auto key = std::invoke(key_gen);

        if ( auto cache_res = at(key); cache_res.hit() ) {
            return false;
        }

        do_generate(std::forward<KeyGen>(key_gen),
            std::forward<ValGen>(val_gen),
            std::forward<Args>(args)...
        );

        return true;
    }

    [[maybe_unused]] cache_result operator[](const key_type& key)
        requires requires (const key_type& k) { get(key); } {
        return get(key);
    }

    [[maybe_unused]] const cache_result operator[](const key_type& key) const
        requires requires (const key_type& k) { get(key); } {
        return const_cast<my_type*>(this)->operator[](key);
    }

    size_type size() const noexcept {
        return LRU_list_.size();
    }

    size_type capacity() const noexcept {
        return cacheline_size() * num_cacheline();
    }

    template <class PairLike>
    bool insert(PairLike&& p)
        requires std::constructible_from<value_type,
            decltype(p.first), decltype(p.second)
        > || std::constructible_from<value_type,
            decltype(std::move(p.first)), decltype(std::move(p.second))
        > {
        return insert( cbegin(), std::forward<PairLike>(p) );
    }

    template <class PairLike>
    bool insert(const_iterator pos, PairLike&& p)
        requires std::constructible_from<value_type,
            decltype(p.first), decltype(p.second)
        > || std::constructible_from<value_type,
            decltype(std::move(p.first)), decltype(std::move(p.second))
        > {
        if constexpr (std::is_lvalue_reference_v<PairLike>) {
            return try_emplace( pos, p.first, p.second );
        }
        else {
            return try_emplace( pos, std::move(p.first), std::move(p.second) );
        }
    }

    template <class MappedT>
        requires std::constructible_from<mapped_type, MappedT>
    bool insert_or_assign( const key_type& key, 
        MappedT&& mapped_value
    ) {
        return insert_or_assign_impl( key,
            std::forward<MappedT>(mapped_value)
        );
    }

    template <class MappedT>
        requires std::constructible_from<mapped_type, MappedT>
    bool insert_or_assign( key_type&& key,
        MappedT&& mapped_value
    ) {
        return insert_or_assign_impl( std::move(key),
            std::forward<MappedT>(mapped_value)
        );
    }

    template <class KeyU, class ... Args>
        requires std::equality_comparable_with<const KeyU&, const key_type&>
            && std::constructible_from<key_type, KeyU>
            && std::constructible_from<mapped_type, Args...>
    bool try_emplace( KeyU&& key, Args&& ... args ) {
        return try_emplace( cbegin(), std::forward<KeyU>(key),
            std::forward<Args>(args)...
        );
    }

    template <class KeyU, class ... Args>
        requires std::equality_comparable_with<const KeyU&, const key_type&>
            && std::constructible_from<key_type, KeyU>
            && std::constructible_from<mapped_type, Args...>
    bool try_emplace( const_iterator pos, KeyU&& key, Args&& ... args ) {
        if (at(key).hit()) {
            return false;
        }

        do_emplace( pos, std::forward<KeyU>(key),
            std::forward<Args>(args)...
        );

        return true;
    }

    template <class ... Args>
        requires std::constructible_from<value_type, Args...>
    bool emplace(Args&& ... args) {
        return emplace( cbegin(), std::forward<Args>(args)... );
    }

    template <class ... Args>
        requires std::constructible_from<value_type, Args...>
    bool emplace(const_iterator pos, Args&& ... args) {
        auto tmp = value_type(std::forward<Args>(args)...);
        auto& key = tmp.first;

        if (at(key).hit()) {
            return false;
        }

        do_insert(pos, std::move(tmp));

        return true;
    }

    size_type cacheline_size() const noexcept {
        return cacheline_size_;
    }

    size_type num_cacheline() const noexcept {
        return num_cacheline_;
    }

    void adjust() {
        if (size() <= capacity()) {
            return;
        }

        auto num_overflow_cacheline = std::max<size_type>(
            (size() + cacheline_size() - 1) / cacheline_size(), 0
        );

        // for poor support of list's erasing range in C++20,
        // use subrange and its iterators alternatively.
        auto invalidate_range = std::ranges::subrange{
            std::prev( LRU_list_.end(), cacheline_size() ),
            LRU_list_.end()
        };

        std::ranges::for_each(invalidate_range,
            [this](const auto& elem) { map_.erase(elem.first); }
        );

        LRU_list_.erase( invalidate_range.begin(), invalidate_range.end() );

        check_invariant();
    }

    void clear() {
        LRU_list_.clear();
        map_.clear();
        check_invariant();
    }

    iterator find(const key_type& key) {
        return find_impl(key);
    }

    const_iterator find(const key_type& key) const {
        return find_impl(key);
    }

    template <class KeyU>
        requires std::equality_comparable_with<const KeyU&, const key_type&>
    iterator find(const KeyU& key) {
        return find_impl(key);
    }

    template <class KeyU>
        requires std::equality_comparable_with<const KeyU&, const key_type&>
    const_iterator find(const KeyU& key) const {
        return find_impl(key);
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

    template <class KeyU>
        requires std::equality_comparable_with<const KeyU&, const key_type&>
    bool contains(const KeyU& key) const {
        return at(key).hit();
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
    template <class KeyU>
    std::optional<ref_mapped_proxy> at_impl(const KeyU& key) const {
        if (!map_.contains(key)) {
            return {};
        }

        // - use std::list<T>::splice to not restore memory but
        //   just reset nodes' links.
        // - use std::unordered_map<...>::find to transparently compare
        //   given arguments with stored keys
        //   without construction of temporary object.
        //   std::unordered_map<...>::at receives const key_type&,
        //   so that it constructs a temporary object
        //   if the argument's type is not a key_type.
        LRU_list_.splice(
            LRU_list_.begin(), LRU_list_, map_.find(key)->second
        );

        check_invariant();

        return LRU_list_.front().second;
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
        requires std::constructible_from<value_type, Args...> 
    void do_emplace(const_iterator pos, Args&& ... args) {
        const auto pos_list = pos.base();
        const auto it_emplaced = LRU_list_.emplace(
            pos_list, std::forward<Args>(args)...
        );
        map_.emplace(it_emplaced->first, it_emplaced);

        adjust();
        check_invariant();
    }

    template <class PairLike>
        requires requires (const_iterator i, PairLike&& p) {
            do_emplace( i, p.first, p.second );
        } || requires (const_iterator i, PairLike&& p) {
            do_emplace( i, std::move(p.first), std::move(p.second) );
        }
    void do_insert(const_iterator pos, PairLike&& val) {
        const auto list_it = pos.base();

        if constexpr (std::is_lvalue_reference_v<PairLike>) {
            do_emplace( pos, val.first, val.second );
        }
        else {
            do_emplace( pos, std::move(val.first), std::move(val.second) );
        }
    }

    template <class KeyU, class ValGen, class ... Args>
        requires std::constructible_from<key_type, KeyU>
            && std::invocable<ValGen, Args...>
            && (!std::same_as< std::remove_cvref_t<ValGen>, mapped_type >)
            && std::constructible_from<
                mapped_type, std::invoke_result_t<ValGen, Args...>
            >
    void do_generate(KeyU&& key, ValGen&& gen, Args&& ... args) {
        do_generate([&key = std::forward<KeyU>(key)]() mutable {
                if constexpr ( std::is_lvalue_reference_v<KeyU> ) {
                    return key;
                }
                else {
                    return std::move(key);
                }
            },  std::forward<ValGen>(gen), std::forward<Args>(args)...
        );
    }

    template <class KeyGen, class ValGen, class ... Args>
        requires std::invocable<KeyGen>
            && (!std::same_as< std::remove_cvref_t<KeyGen>, key_type >)
            && std::constructible_from< key_type, std::invoke_result_t<KeyGen> >
            && std::invocable<ValGen, Args...>
            && (!std::same_as< std::remove_cvref_t<ValGen>, mapped_type >)
            && std::constructible_from<
                mapped_type, std::invoke_result_t<ValGen, Args...>
            >     
    void do_generate(KeyGen&& key_gen,
        ValGen&& val_gen, Args&& ... args
    ) {
        LRU_list_.emplace_front(
            std::invoke(key_gen),
            std::invoke(std::forward<ValGen>(val_gen),
                std::forward<Args>(args)...
            )
        );
        map_.emplace(std::invoke(key_gen), LRU_list_.begin());

        adjust();
        check_invariant();
    }

    template <class KeyU, class MappedT>
        requires std::constructible_from<key_type, KeyU>
            && std::constructible_from<mapped_type, MappedT>
            && std::copyable<key_type>
    bool insert_or_assign_impl(
        KeyU&& key, MappedT&& mapped_value
    ) {
        if (auto cacheRes = at(key); cacheRes.hit()) {
            *cacheRes = std::forward<MappedT>(mapped_value);
            return false;
        }

        do_emplace( cbegin(), std::forward<KeyU>(key),
            std::forward<MappedT>(mapped_value)
        );

        return true;
    }

    template <class KeyU>
        requires std::equality_comparable_with<const KeyU&, const key_type&>
    iterator find_impl(const KeyU& key) {
        if ( auto found_it = map_.find(key); found_it != map_.end() ) {
            return iterator(*this, found_it->second);
        }

        return end();
    }

    template <class KeyU>
        requires std::equality_comparable_with<const KeyU&, const key_type&>
    const_iterator find_impl(const KeyU& key) const {
        if ( auto found_it = map_.find(key); found_it != map_.end() ) {
            return const_iterator(*this, found_it->second);
        }

        return cend();
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
    mutable map_type map_;
    size_type cacheline_size_;
    size_type num_cacheline_;
};

#endif  // __LRUCache