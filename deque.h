#include <vector>
#include <stdexcept>

template<typename T>
class Deque {
private:
    static const size_t chunk_size = 32;
    std::vector<T*> m_data;
    size_t begin_chunk;
    int begin_ind;
    size_t end_chunk, end_ind;

    //-1 <= begin_ind <= chunk_size - 2
    //0 <= end_ind <= chunk_size - 1

    void clearVectorOfChunks(size_t begin_chunk, int begin_ind, size_t end_chunk, size_t end_ind) noexcept {
        if (begin_chunk == end_chunk) {
            for (size_t j = static_cast<size_t>(begin_ind + 1); j < end_ind; ++j) {
                (m_data[begin_chunk] + j)->~T();
            }
        } else {
            for (size_t i = begin_chunk + 1; i < end_chunk; ++i) {
                for (size_t j = 0; j < chunk_size; ++j) {
                    (m_data[i] + j)->~T();
                }
            }
            for (size_t j = 0; j < end_ind; ++j) {
                (m_data[end_chunk] + j)->~T();
            }
            for (size_t j = static_cast<size_t>(begin_ind + 1); j < chunk_size; ++j) {
                (m_data[begin_chunk] + j)->~T();
            }
        }
        for (size_t i = begin_chunk; i < end_chunk + (end_ind != 0 ? 1 : 0); ++i) {
            ::operator delete[](m_data[i]);
        }
    }

    std::pair<size_t, size_t> getIndex(size_t ind) const noexcept {
        if (ind <= chunk_size - static_cast<size_t>(begin_ind + 2))
            return {begin_chunk, ind + static_cast<size_t>(begin_ind + 1)};
        ind -= (chunk_size - static_cast<size_t>(begin_ind + 1));
        return {begin_chunk + 1 + ind / chunk_size, ind % chunk_size};
    }

    void expandBeginning() noexcept {
        std::vector<T*> new_data(m_data.size() * 2 + 1);
        for (size_t i = 0; i < m_data.size(); ++i)
            std::swap(m_data[i], new_data[i + m_data.size() + 1]);
        begin_chunk += m_data.size() + 1;
        end_chunk += m_data.size() + 1;
        m_data.swap(new_data);
    }

    template<typename... Args>
    void initializeHelper(const Args&... args) {
        m_data.resize(end_chunk + (end_ind != 0 ? 1 : 0));
        size_t chunk_ind = 0, ind = 0;
        try {
            for (; chunk_ind < end_chunk; ++chunk_ind) {
                m_data[chunk_ind] = reinterpret_cast<T*>(new char[sizeof(T) * chunk_size]);
                for (; ind < chunk_size; ++ind) {
                    new(m_data[chunk_ind] + ind) T(args...);
                }
                ind = 0;
            }
            if (end_ind != 0)
                m_data[chunk_ind] = reinterpret_cast<T*>(new char[sizeof(T) * chunk_size]);
            for (; ind < end_ind; ++ind) {
                new(m_data[end_chunk] + ind) T(args...);
            }
        } catch (...) {
            if (ind == 0)
                ::operator delete[](m_data[chunk_ind]);
            clearVectorOfChunks(0, -1, chunk_ind, ind);
            throw;
        }
    }

public:
    using value_type = T;

    Deque() : m_data(), begin_chunk(0), begin_ind(-1),  end_chunk(0), end_ind(0) {}

    Deque(const Deque& other) : m_data(), begin_chunk(other.begin_chunk), begin_ind(other.begin_ind),
                                end_chunk(other.end_chunk), end_ind(other.end_ind) {
        m_data.resize(other.m_data.size());
        for (size_t i = other.begin_chunk; i < other.end_chunk + (other.end_ind != 0 ? 1 : 0); ++i) {
            m_data[i] = reinterpret_cast<T*>(new char[sizeof(T) * chunk_size]);
        }
        if (other.begin_chunk == other.end_chunk) {
            size_t ind = static_cast<size_t>(other.begin_ind + 1);
            try {
                for (; ind < other.end_ind; ++ind)
                    new(m_data[other.begin_chunk] + ind) T(other.m_data[other.begin_chunk][ind]);
            } catch (...) {
                clearVectorOfChunks(other.begin_chunk, other.begin_ind,
                                    other.end_chunk, ind);
                throw;
            }
        } else {
            size_t chunk_ind = other.begin_chunk, ind = static_cast<size_t>(other.begin_ind + 1);
            try {
                for (; ind < chunk_size; ++ind)
                    new(m_data[chunk_ind] + ind) T(other.m_data[chunk_ind][ind]);
                ++chunk_ind;
                ind = 0;
                for (; chunk_ind < other.end_chunk; ++chunk_ind) {
                    for (; ind < chunk_size; ++ind)
                        new(m_data[chunk_ind] + ind) T(other.m_data[chunk_ind][ind]);
                    ind = 0;
                }
                for (; ind < other.end_ind; ++ind) {
                    new(m_data[chunk_ind] + ind) T(other.m_data[chunk_ind][ind]);
                }
            } catch (...) {
                clearVectorOfChunks(other.begin_chunk, other.begin_ind,
                                    chunk_ind, ind);
                throw;
            }
        }
    }

    Deque(size_t count, const T& value): m_data(), begin_chunk(0), begin_ind(-1),
                                         end_chunk(count / chunk_size), end_ind(count % chunk_size) {
        initializeHelper(value);
    }

    Deque(size_t count): m_data(), begin_chunk(0), begin_ind(-1),
                         end_chunk(count / chunk_size), end_ind(count % chunk_size) {
        initializeHelper();
    }

    void swap(Deque& other) noexcept {
        m_data.swap(other.m_data);
        std::swap(begin_chunk, other.begin_chunk);
        std::swap(begin_ind, other.begin_ind);
        std::swap(end_chunk, other.end_chunk);
        std::swap(end_ind, other.end_ind);
    }

    Deque& operator=(const Deque &other) {
        Deque temp(other);
        swap(temp);
        return *this;
    }

    ~Deque() {
        clearVectorOfChunks(begin_chunk, begin_ind, end_chunk, end_ind);
    }

    size_t size() const noexcept {
        if (begin_chunk == end_chunk) {
            return end_ind - static_cast<size_t>(begin_ind + 1);
        }
        return end_ind + (chunk_size - static_cast<size_t>(begin_ind + 1)) +
               (end_chunk - begin_chunk - 1) * chunk_size;
    }

    T& operator[](size_t ind) noexcept {
        auto [chunk_number, index_number] = getIndex(ind);
        return m_data[chunk_number][index_number];
    }

    const T& operator[](size_t ind) const noexcept {
        auto [chunk_number, index_number] = getIndex(ind);
        return m_data[chunk_number][index_number];
    }

    T& at(size_t ind) {
        auto [chunk_number, index_number] = getIndex(ind);
        if (chunk_number > end_chunk || (chunk_number == end_chunk && index_number >= end_ind))
            throw std::out_of_range("Deque index out of range");
        return m_data[chunk_number][index_number];
    }

    const T& at(size_t ind) const {
        auto [chunk_number, index_number] = getIndex(ind);
        if (chunk_number > end_chunk || (chunk_number == end_chunk && index_number >= end_ind))
            throw std::out_of_range("Deque index out of range");
        return m_data[chunk_number][index_number];
    }

    void push_back(const T& element) {
        if (end_ind == 0) {
            T *temp_chunk = reinterpret_cast<T*>(new char[sizeof(T) * chunk_size]);
            try {
                new(temp_chunk) T(element);
            } catch (...) {
                ::operator delete[](temp_chunk);
                throw;
            }
            m_data.push_back(temp_chunk);
        } else {
            new(m_data[end_chunk] + end_ind) T(element);
        }
        ++end_ind;
        if (end_ind == chunk_size) {
            end_ind = 0;
            ++end_chunk;
        }
    }

    void pop_back() noexcept {
        if (end_ind == 0) {
            end_ind = chunk_size - 1;
            --end_chunk;
        } else {
            --end_ind;
        }
        (m_data[end_chunk] + end_ind)->~T();
        if (end_ind == 0) {
            ::operator delete[](m_data.back());
            m_data.pop_back();
        }
    }

    void push_front(const T& element) {
        if (begin_ind == -1) {
            T* temp_chunk = reinterpret_cast<T*>(new char[sizeof(T) * chunk_size]);
            try {
                new(temp_chunk + (chunk_size - 1)) T(element);
            } catch (...) {
                ::operator delete[](temp_chunk);
                throw;
            }
            if (begin_chunk == 0 && begin_ind == -1)
                expandBeginning();
            begin_ind = chunk_size - 1;
            --begin_chunk;
            m_data[begin_chunk] = temp_chunk;
        } else {
            new(m_data[begin_chunk] + begin_ind) T(element);
        }
        --begin_ind;
    }

    void pop_front() noexcept {
        ++begin_ind;
        (m_data[begin_chunk] + begin_ind)->~T();
        if (begin_ind == chunk_size - 1) {
            ::operator delete[](m_data[begin_chunk]);
            ++begin_chunk;
            begin_ind = -1;
        }
    }

    template<bool IsConst>
    struct BaseIterator {
    private:
        using vector_type = std::conditional_t<IsConst, const std::vector<T*>, std::vector<T*>>;
        vector_type* m_data;
        size_t chunk_index;
        T* m_element;
        size_t element_index;

    public:
        using difference_type = std::ptrdiff_t;
        using value_type = std::conditional_t<IsConst, const T, T>;
        using reference = value_type&;
        using iterator_category = std::random_access_iterator_tag;
        using pointer = value_type*;

        BaseIterator(vector_type* m_data, size_t chunk_index, T* m_element, size_t element_index) :
                m_data(m_data), chunk_index(chunk_index), m_element(m_element), element_index(element_index) {}

        operator BaseIterator<true>() const {
            return BaseIterator<true>(m_data, chunk_index, m_element, element_index);
        }

        value_type& operator*() {
            return *m_element;
        }

        value_type *operator->() {
            return m_element;
        }

        BaseIterator& operator++() {
            return *this += 1;
        }

        BaseIterator operator++(int) {
            auto copy = *this;
            ++(*this);
            return copy;
        }

        BaseIterator& operator--() {
            return *this -= 1;
        }

        BaseIterator operator--(int) {
            auto copy = *this;
            --(*this);
            return copy;
        }

        BaseIterator& operator+=(difference_type val) {
            if (val >= 0) {
                auto diff = static_cast<size_t>(val);
                chunk_index += (diff + element_index) / chunk_size;
                element_index = (diff + element_index) % chunk_size;
            }
            else {
                auto chunk_sz = static_cast<difference_type>(chunk_size);
                auto element_ind = static_cast<difference_type>(element_index);
                chunk_index -= static_cast<size_t>((-val - element_ind + chunk_sz - 1) / chunk_sz);
                element_index = static_cast<size_t>(((element_ind + val) % chunk_sz + chunk_sz) % chunk_sz);
            }
            if (m_data->size() > chunk_index)
                m_element = (m_data->operator[](chunk_index) + element_index);
            else
                m_element = nullptr;
            return *this;
        }

        BaseIterator& operator-=(difference_type diff) {
            return *this += (-diff);
        }

        BaseIterator operator+(difference_type diff) const {
            BaseIterator copy(*this);
            copy += diff;
            return copy;
        }

        BaseIterator operator-(difference_type diff) const {
            BaseIterator copy(*this);
            copy -= diff;
            return copy;
        }

        bool operator==(BaseIterator other) const {
            return m_data == other.m_data && chunk_index == other.chunk_index
                   && element_index == other.element_index;
        }

        std::strong_ordering operator<=>(BaseIterator other) const {
            if (chunk_index != other.chunk_index)
                return (chunk_index <=> other.chunk_index);
            return (element_index <=> other.element_index);
        }

        difference_type operator-(BaseIterator other) const {
            if (chunk_index == other.chunk_index)
                return static_cast<difference_type>(element_index) - static_cast<difference_type>(other.element_index);
            if (chunk_index > other.chunk_index)
                return static_cast<difference_type>(element_index
                                                    + (chunk_index - other.chunk_index - 1) * chunk_size + (chunk_size - other.element_index));
            return -static_cast<difference_type>(other.element_index
                                                 + (other.chunk_index - chunk_index - 1) * chunk_size + (chunk_size - element_index));
        }
    };

    using iterator = BaseIterator<false>;
    using const_iterator = BaseIterator<true>;

    iterator begin() {
        T* element = nullptr;
        if (m_data.size() > begin_chunk)
            element = (m_data[begin_chunk] + static_cast<size_t>(begin_ind + 1));
        return iterator(&m_data, begin_chunk, element, static_cast<size_t>(begin_ind + 1));
    }

    const_iterator begin() const {
        T* element = nullptr;
        if (m_data.size() > begin_chunk)
            element = (m_data[begin_chunk] + static_cast<size_t>(begin_ind + 1));
        return const_iterator(&m_data, begin_chunk, element, static_cast<size_t>(begin_ind + 1));
    }

    const_iterator cbegin() const {
        return begin();
    }

    iterator end() {
        return begin() + static_cast<typename iterator::difference_type>(size());
    }

    const_iterator end() const {
        return begin() + static_cast<typename iterator::difference_type>(size());
    }

    const_iterator cend() const {
        return cbegin() + static_cast<typename iterator::difference_type>(size());
    }

    auto rbegin() {
        return std::reverse_iterator<iterator>(end());
    }

    auto rbegin() const {
        return std::reverse_iterator<const_iterator>(end());
    }

    auto rend() {
        return std::reverse_iterator<iterator>(begin());
    }

    auto rend() const {
        return std::reverse_iterator<const_iterator>(begin());
    }

    auto crbegin() const {
        return std::reverse_iterator<const_iterator>(end());
    }

    auto crend() const {
        return std::reverse_iterator<const_iterator>(begin());
    }

    auto insert(Deque::iterator place, const T& val) {
        if (place == begin()) {
            push_front(val);
            return begin();
        }
        auto now = end();
        push_back(val);
        for (auto it = now; it != place; --it) {
            std::swap(*it, *(it - 1));
        }
        return place;
    }

    void erase(Deque::iterator place) {
        if (place == (--end())) {
            pop_back();
            return;
        }
        for (auto it = place; it != begin(); --it) {
            std::swap(*it, *(it - 1));
        }
        pop_front();
    }
};

