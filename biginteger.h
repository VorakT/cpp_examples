#include <algorithm>
#include <cmath>
#include <compare>
#include <complex>
#include <iostream>
#include <string>
#include <vector>


class BigInteger {
  friend std::strong_ordering operator<=>(const BigInteger&, const BigInteger&);
  friend BigInteger operator"" _bi(unsigned long long);
private:
  const static int base = 1000;
  std::vector<int> numbers; // numbers from 0 to 999
  bool negative = false;
  constexpr const static int ten_pwr[4] = {1, 10, 100, 1000};
  const static size_t amount_of_digits = 3;

  void fast_fourier_transform(std::vector<std::complex<long double>>& coef) {
    size_t log_size = 0;
    while (static_cast<size_t>(1 << log_size) < coef.size()) {
      ++log_size;
    }
    for (size_t i = 0; i < coef.size(); ++i) {
      size_t rev_i = 0;
      for (size_t j = 0; j < log_size; ++j) {
        if ((i >> j) & 1) {
          rev_i += (1 << (log_size - j - 1));
        }
      }
      if (i < rev_i) {
        std::swap(coef[i], coef[rev_i]);
      }
    }
    for (size_t len = 2; len <= coef.size(); len *= 2) {
      std::complex<long double> wlen(cosl(2 * acosl(-1) / len), sinl(2 * acosl(-1) / len));
      for (size_t st = 0; st < coef.size(); st += len) {
        std::complex<long double> wlen_deg(1);
        auto left_ptr = coef.begin() + static_cast<int>(st);
        auto right_ptr = coef.begin() + static_cast<int>(st + len / 2);
        for (; left_ptr != coef.begin() + static_cast<int>(st + len / 2);
               ++left_ptr, ++right_ptr) {
          auto temp1 = *left_ptr;
          auto temp2 = (*right_ptr) * wlen_deg;
          *left_ptr = temp1 + temp2;
          *right_ptr = temp1 - temp2;
          wlen_deg *= wlen;
        }
      }
    }
  }

  void abs_add(const BigInteger& bi, bool subtract=false) {
    int left_over = 0;
    if (!subtract) {
      size_t new_size = std::max(numbers.size(), bi.numbers.size());
      numbers.resize(new_size, 0);
    }
    int sign = subtract ? -1 : 1;
    for (size_t i = 0; i < bi.numbers.size(); ++i) {
      numbers[i] += sign * (bi.numbers[i] + left_over);
      left_over = 0;
      if (numbers[i] < 0 || numbers[i] >= base) {
        numbers[i] -= sign * base;
        left_over = 1;
      }
    }
    if (left_over) {
      for (size_t i = bi.numbers.size(); i < numbers.size(); ++i) {
        numbers[i] += sign;
        if (numbers[i] >= 0 && numbers[i] < base) {
          left_over = 0;
          break;
        }
        numbers[i] = subtract ? base - 1 : 0;
      }
      if (left_over && !subtract) {
        numbers.push_back(1);
      }
    }
    while (numbers.back() == 0 && numbers.size() > 1) {
      numbers.pop_back();
    }
    if (is_null()) {
      negative = false;
    }
  }

  BigInteger small_prod(int small_number) const { //0 <= small_number < 1000
    BigInteger copy = (*this);
    int left_over = 0;
    for (size_t i = 0; i < copy.numbers.size(); ++i) {
      left_over += copy.numbers[i] * small_number;
      copy.numbers[i] = left_over % base;
      left_over /= base;
    }
    if (left_over) {
      copy.numbers.push_back(left_over);
    }
    while (copy.numbers.back() == 0 && copy.numbers.size() > 1) {
      copy.numbers.pop_back();
    }
    copy.negative = false;
    return copy;
  }

  void prod_by_base() {
    if (!is_null()) {
      numbers.insert(numbers.begin(), 0);
    }
  }

  static std::strong_ordering vector_comparison(const std::vector<int>& v1, const std::vector<int>& v2) {
    if (v1.size() != v2.size()) {
      return (v1.size() <=> v2.size());
    }
    size_t ind = 0;
    while (ind < v1.size() && v1[v1.size() - 1 - ind] == v2[v2.size() - 1 - ind]) {
      ++ind;
    }
    if (ind == v1.size()) {
      return std::strong_ordering::equal;
    }
    return (v1[v1.size() - 1 - ind] <=> v2[v2.size() - 1 - ind]);
  }

  BigInteger& division(const BigInteger& bi, bool mod);

public:
  BigInteger() = default;

  BigInteger(const std::string& str, size_t sz=0) : negative(false) {
    if (str[0] == '-') {
      negative = true;
    }
    sz = str.size();
    size_t zeros = 0;
    for (size_t i = static_cast<size_t>(negative); i < sz; ++i) {
      if (str[i] != '0') {
        break;
      }
      ++zeros;
    }
    if (static_cast<size_t>(negative) + zeros == sz) {
      numbers = { 0 };
      return;
    }
    numbers.resize((sz - static_cast<size_t>(negative) - zeros + 2) /
                   amount_of_digits, 0);
    for (size_t ind = 0; ind + zeros + static_cast<size_t>(negative) < sz; ++ind) {
      numbers[ind / amount_of_digits] +=
        (str[sz - 1 - ind] - '0') * ten_pwr[ind % amount_of_digits];
    }
    if (is_null()) {
      negative = false;
    }
  }

  BigInteger(int n) {
    if (n < 0) {
      negative = true;
      n *= -1;
    }
    while (n > 0) {
      numbers.push_back(n % ten_pwr[amount_of_digits]);
      n /= ten_pwr[amount_of_digits];
    }
    if (numbers.empty()) {
      numbers.push_back(0);
    }
  }

  explicit BigInteger(std::vector<int>& v): numbers(v) {}

  bool operator==(const BigInteger&) const = default;

  void change_sign() {
    if (!is_null()) {
      negative ^= 1;
    }
  }

  std::string toString() const {
    std::string res = "";
    if (negative) {
      res += '-';
    }
    for (auto it = numbers.rbegin(); it != numbers.rend(); ++it) {
      std::string str = std::to_string(*it);
      if (it != numbers.rbegin()) {
        for (size_t i = str.size(); i < amount_of_digits; ++i) {
          res += '0';
        }
      }
      res += str;
    }
    return res;
  }

  BigInteger& operator*=(const BigInteger& bi) {
    size_t pwr_of_2 = 1;
    while (pwr_of_2 < std::max(numbers.size(), bi.numbers.size())) {
      pwr_of_2 *= 2;
    }
    pwr_of_2 *= 2;
    std::vector<std::complex<long double>> res(pwr_of_2, 0);
    for (size_t i = 0; i < res.size(); ++i) {
      res[i] = std::complex<long double>((i < numbers.size() ? numbers[i] : 0),
                                         (i < bi.numbers.size() ? bi.numbers[i] : 0));
    }
    fast_fourier_transform(res);
    for (size_t i = 0; i < res.size(); ++i) {
      res[i] = res[i] * res[i] / static_cast<std::complex<long double>>(res.size());
    }
    fast_fourier_transform(res);
    std::reverse(res.begin() + 1, res.end());
    numbers.resize(res.size());
    long long left_over = 0;
    for (size_t i = 0; i < res.size(); ++i) {
      left_over += static_cast<long long>(std::round(res[i].imag() / 2));
      numbers[i] = static_cast<int>(left_over % ten_pwr[amount_of_digits]);
      left_over /= ten_pwr[amount_of_digits];
    }
    while (left_over != 0) {
      numbers.push_back(static_cast<int>(left_over % ten_pwr[amount_of_digits]));
      left_over /= ten_pwr[amount_of_digits];
    }
    while (numbers.back() == 0 && numbers.size() > 1) {
      numbers.pop_back();
    }
    numbers.shrink_to_fit();
    negative ^= bi.negative;
    if (is_null()) {
      negative = false;
    }
    return *this;
  }

  bool is_null() const {
    return (numbers.size() == 1 && numbers[0] == 0);
  }

  bool is_negative() const {
    return negative;
  }

  BigInteger abs() const {
    BigInteger copy = (*this);
    copy.negative = false;
    return copy;
  }

  void swap(BigInteger& bi) {
    std::swap(negative, bi.negative);
    numbers.swap(bi.numbers);
  }

  BigInteger& operator+=(BigInteger bi);

  BigInteger& operator-=(BigInteger bi) {
    change_sign();
    (*this) += bi;
    change_sign();
    if (is_null()) {
      negative = false;
    }
    return (*this);
  }

  BigInteger& operator++() {
    (*this) += 1;
    return *this;
  }

  BigInteger operator++(int) {
    BigInteger copy = (*this);
    (*this) += 1;
    return copy;
  }

  BigInteger& operator--() {
    (*this) -= 1;
    return (*this);
  }

  BigInteger operator--(int) {
    BigInteger copy = (*this);
    (*this) -= 1;
    return copy;
  }

  BigInteger operator-() const {
    BigInteger copy = *this;
    if (!is_null()) {
      copy.change_sign();
    }
    return copy;
  }

  explicit operator bool() const;

  BigInteger& operator/= (const BigInteger& bi);

  BigInteger& operator%=(const BigInteger&);
};

std::strong_ordering operator<=>(const BigInteger& bi1, const BigInteger& bi2) {
  auto sign_ordering = bi2.negative <=> bi1.negative;
  if (sign_ordering != std::strong_ordering::equal) {
    return sign_ordering;
  }
  return !bi1.negative ? BigInteger::vector_comparison(bi1.numbers, bi2.numbers) :
         BigInteger::vector_comparison(bi2.numbers, bi1.numbers);
}

BigInteger& BigInteger::division(const BigInteger &bi, bool mod=false) {
  if (numbers.size() < bi.numbers.size()) {
    if (!mod) {
      (*this) = 0;
    }
    return (*this);
  }
  std::vector<int> res;
  std::vector<int> now(bi.numbers.size());
  for (size_t i = 0; i < bi.numbers.size(); ++i) {
    now[now.size() - 1 - i] = numbers[numbers.size() - 1 - i];
  }
  BigInteger now_bi(now);
  for (size_t i = 0; i <= numbers.size() - bi.numbers.size(); ++i) {
    int left = 0, right = BigInteger::base;
    while (right - left > 1) {
      int middle = (left + right) / 2;
      if (now_bi >= bi.small_prod(middle)) {
        left = middle;
      }
      else {
        right = middle;
      }
    }
    res.push_back(left);
    now_bi -= bi.small_prod(left);
    if (i != numbers.size() - bi.numbers.size()) {
      now_bi.prod_by_base();
      now_bi += numbers[numbers.size() - 1 - bi.numbers.size() - i];
    }
  }
  if (mod) {
    bool sign = negative;
    *this = now_bi;
    if (sign) {
    	*this -= bi;
    }
    return *this;
  }
  std::reverse(res.begin(), res.end());
  while (res.back() == 0 && res.size() > 1) {
    res.pop_back();
  }
  bool sign = negative ^ bi.negative;
  (*this) = BigInteger(res);
  if (sign) {
    change_sign();
  }
  if (is_null()) {
    negative = false;
  }
  return *this;
}

BigInteger& BigInteger::operator/= (const BigInteger& bi) {
  return division(bi);
}

std::istream& operator>>(std::istream& is, BigInteger& bi) {
  std::string str;
  is >> str;
  bi = BigInteger(str);
  return is;
}

std::ostream& operator<<(std::ostream& os, const BigInteger& bi) {
  os << bi.toString();
  return os;
}

BigInteger operator*(const BigInteger& bi1, BigInteger bi2) {
  bi2 *= bi1;
  return bi2;
}

BigInteger& BigInteger::operator+=(BigInteger bi) {
  if (negative == bi.negative) {
    abs_add(bi);
    return *this;
  }
  bi.change_sign();
  if (!(!negative ^ ((*this) >= bi))) {
    abs_add(bi, true);
    return *this;
  }
  bi.abs_add(*this, true);
  (*this) = bi;
  change_sign();
  if (is_null()) {
    negative = false;
  }
  return *this;
}

BigInteger operator+(const BigInteger& bi1, const BigInteger& bi2) {
  BigInteger res = bi1;
  res += bi2;
  return res;
}

BigInteger operator-(const BigInteger& bi1, const BigInteger& bi2) {
  BigInteger res = bi1;
  res -= bi2;
  return res;
}

BigInteger::operator bool() const {
  return (*this) != 0;
}

BigInteger operator"" _bi (const char* digits, size_t sz) {
  return BigInteger(digits, sz);
}

BigInteger operator"" _bi(unsigned long long number) {
  std::vector<int> digits;
  while (number > 0) {
    digits.push_back(static_cast<int>(number % BigInteger::base));
    number /= BigInteger::base;
  }
  if (digits.empty()) {
    digits.push_back(0);
  }
  return BigInteger(digits);
}

BigInteger operator/(const BigInteger& bi1, const BigInteger& bi2) {
  BigInteger res = bi1;
  res /= bi2;
  return res;
}

BigInteger& BigInteger::operator%=(const BigInteger& bi) {
  return division(bi, true);
}

BigInteger operator%(const BigInteger& bi1, const BigInteger& bi2) {
  BigInteger res = bi1;
  res %= bi2;
  return res;
}

BigInteger pwr(BigInteger bi, size_t n) {
  BigInteger res = 1;
  while (n > 0) {
    if (n % 2) {
      res *= bi;
    }
    bi *= bi;
    n /= 2;
  }
  return res;
}

BigInteger gcd(BigInteger bi1, BigInteger bi2) {
  while(bi1 != 0) {
    bi2 %= bi1;
    bi1.swap(bi2);
  }
  return bi2.abs();
}

class Rational {
private:
  BigInteger numerator, denominator;

  void reduce() {
    BigInteger bi = gcd(numerator, denominator);
    numerator /= bi;
    denominator /= bi;
  }

  void make_standard() {
    reduce();
    if (denominator.is_negative()) {
      denominator.change_sign();
      numerator.change_sign();
    }
  }

public:
  Rational() = default;

  Rational(const BigInteger& bi): numerator(bi), denominator(1) {}

  Rational(int number) {
    numerator = number;
    denominator = 1;
  }

  const BigInteger& get_numerator() const {
    return numerator;
  }

  const BigInteger& get_denominator() const {
    return denominator;
  }

  bool is_negative() const {
    return numerator.is_negative();
  }

  Rational& operator+=(const Rational& rt) { //rtrtrtrtrtrtrtrtrtrtrtrt
    numerator = numerator * rt.denominator +  denominator * rt.numerator;
    denominator *= rt.denominator;
    make_standard();
    return *this;
  }

  Rational& operator-=(const Rational& rt) {
    numerator = numerator * rt.denominator - denominator * rt.numerator;
    denominator *= rt.denominator;
    make_standard();
    return *this;
  }

  Rational& operator/=(const Rational& rt) {
    BigInteger copy = rt.numerator;
    numerator *= rt.denominator;
    denominator *= copy;
    make_standard();
    return *this;
  }

  Rational& operator*=(const Rational& rt) {
    numerator *= rt.numerator;
    denominator *= rt.denominator;
    make_standard();
    return *this;
  }

  Rational operator-() const {
    Rational copy = *this;
    copy.numerator.change_sign();
    return copy;
  }

  std::string toString() {
    std::string res = numerator.toString();
    if (denominator != 1) {
      res += '/';
      res += denominator.toString();
    }
    return res;
  }

  std::string asDecimal(size_t precision=0) {
    if (precision == 0) {
      return (numerator / denominator).toString();
    }
    const int ten = 10;
    std::string res = (numerator * pwr(ten, precision) / denominator).toString();
    bool negative = false;
    if (res[0] == '-') {
      negative = true;
      res.erase(res.begin());
    }
    std::string nulls = "";
    while (res.size() + nulls.size() < precision + 1) {
      nulls += '0';
    }
    res = nulls + res;
    res.insert(res.end() - static_cast<int>(precision), '.');
    if (negative) {
      res.insert(res.begin(), '-');
    }
    return res;
  }

  explicit operator double() {
    const size_t max_accuracy = 310;
    std::string str = asDecimal(max_accuracy);
    return std::stod(str);
  }

  bool operator==(const Rational&) const = default;
};

Rational operator+(const Rational& rt1, const Rational& rt2) {
  Rational res = rt1;
  res += rt2;
  return res;
}

Rational operator-(const Rational& rt1, const Rational& rt2) {
  Rational res = rt1;
  res -= rt2;
  return res;
}

Rational operator/(const Rational& rt1, const Rational& rt2) {
  Rational res = rt1;
  res /= rt2;
  return res;
}

Rational operator*(const Rational& rt1, const Rational& rt2) {
  Rational res = rt1;
  res *= rt2;
  return res;
}

std::strong_ordering operator<=>(const Rational& rt1, const Rational& rt2) {
  Rational dif = rt1 - rt2;
  if (dif == 0) {
    return std::strong_ordering::equal;
  }
  if (dif.is_negative()) {
    return std::strong_ordering::less;
  }
  return std::strong_ordering::greater;
}

std::istream& operator>>(std::istream& is, Rational& rt) {
  std::string str;
  is >> str;
  rt = Rational(str);
  return is;
}
