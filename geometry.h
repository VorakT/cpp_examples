#include <iostream>
#include <cmath>
#include <utility>
#include <vector>
#include <algorithm>


bool areEqualUpToError(double d1, double d2) {
  const double error = 0.00000001;
  return std::abs(d1 - d2) < error;
}

double toRadians(double angle) {
  const int pi_in_degrees = 180;
  return angle * std::acos(-1) / pi_in_degrees;
}

double toDegrees(double angle) {
  const int pi_in_degrees = 180;
  return angle * pi_in_degrees / std::acos(-1);
}


class Line;

struct Point {
  double x, y;

  Point() = default;

  Point(double x, double y): x(x), y(y) {}

  void operator-=(const Point& other) {
    x -= other.x;
    y -= other.y;
  }

  Point operator-(const Point& other) const {
    Point result = *this;
    result -= other;
    return result;
  }

  void operator+=(const Point& other) {
    x += other.x;
    y += other.y;
  }

  Point operator+(Point other) const {
    other += *this;
    return other;
  }

  Point operator*(double d) const {
    Point point = *this;
    point.x *= d;
    point.y *= d;
    return point;
  }

  Point operator/(double d) const {
    Point point = *this;
    point.x /= d;
    point.y /= d;
    return point;
  }

  void rotate(const Point& center, double angle) {
    angle = toRadians(angle);
    *this -= center;
    Point new_point(std::cos(angle) * x - std::sin(angle) * y,
      std::sin(angle) * x + std::cos(angle) * y);
    *this = new_point + center;
  }

  void reflect(const Point& center) {
    *this = center + center - *this;
  }

  Point projection(const Line&) const;

  void reflect(const Line&);

  void scale(const Point& center, double coefficient) {
    *this = center + (*this - center) * coefficient;
  }
};

bool operator==(const Point& point1, const Point& point2) {
  return areEqualUpToError(point1.x, point2.x) && areEqualUpToError(point1.y, point2.y);
}

double length(const Point& point) {
  return std::sqrt(point.x * point.x + point.y * point.y);
}

double distance_between_points(const Point& point1, const Point& point2) {
  return length(point1 - point2);
}

double scalarProduct(const Point& point1, const Point& point2) {
  return point1.x * point2.x + point1.y * point2.y;
}

double vectorProduct(const Point& point1, const Point& point2) {
  return point1.x * point2.y - point2.x * point1.y;
}

bool areTrianglesEqual(const Point& tr1_pt1, const Point& tr1_pt2, const Point& tr1_pt3, const Point& tr2_pt1,
                       const Point& tr2_pt2, const Point& tr2_pt3) {
  Point tr1_vector1 = tr1_pt2 - tr1_pt1, tr1_vector2 = tr1_pt3 - tr1_pt1;
  Point tr2_vector1 = tr2_pt2 - tr2_pt1, tr2_vector2 = tr2_pt3 - tr2_pt1;
  return areEqualUpToError(length(tr1_vector1), length(tr2_vector1)) &&
         areEqualUpToError(length(tr1_vector2), length(tr2_vector2)) &&
         areEqualUpToError(scalarProduct(tr1_vector1, tr1_vector2), scalarProduct(tr2_vector1, tr2_vector2));
}

bool areTrianglesSimilar(const Point& tr1_pt1, const Point& tr1_pt2, const Point& tr1_pt3, const Point& tr2_pt1,
                         const Point& tr2_pt2, const Point& tr2_pt3, double coefficient) {
  Point tr1_vector1 = tr1_pt2 - tr1_pt1, tr1_vector2 = tr1_pt3 - tr1_pt1;
  Point tr2_vector1 = tr2_pt2 - tr2_pt1, tr2_vector2 = tr2_pt3 - tr2_pt1;
  return areEqualUpToError(length(tr1_vector1), length(tr2_vector1) * coefficient) &&
         areEqualUpToError(length(tr1_vector2), length(tr2_vector2) * coefficient) &&
         areEqualUpToError(scalarProduct(tr1_vector1, tr1_vector2),
                           scalarProduct(tr2_vector1, tr2_vector2) * coefficient * coefficient);
}


class Line {
private:
  double a, b, c;

public:
  Line() = default;

  Line(double slope, double shift): a(slope), b(-1), c(shift) {}

  Line(const Point& point1, const Point& point2) : a(point2.y - point1.y), b(point1.x - point2.x),
    c(point1.x * (point1.y - point2.y) + point1.y * (point2.x - point1.x)) {}

  Line(const Point& pt, double slope): a(slope), b(-1), c(pt.y - pt.x * slope) {}
  
  Line(double a, double b, double c) : a(a), b(b), c(c) {}

  double get_a() const {
    return a;
  }

  double get_b() const {
    return b;
  }

  double get_c() const {
    return c;
  }

  Point intersection(const Line& other) const {
    return Point((b * other.c - other.b * c) / (a * other.b - other.a * b),
                 (c * other.a - other.c * a) / (a * other.b - other.a * b));
  }
};

bool operator==(const Line& line1, const Line& line2) {
  double coef = 0;
  if ((areEqualUpToError(line1.get_a(), 0) || areEqualUpToError(line2.get_a(), 0)) &&
      !areEqualUpToError(line1.get_a(), line2.get_a())) {
    return false;
  }
  if (!areEqualUpToError(line1.get_a(), 0) && !areEqualUpToError(line2.get_a(), 0)) {
    coef = line1.get_a() / line2.get_a();
  }
  if ((areEqualUpToError(line1.get_b(), 0) || areEqualUpToError(line2.get_b(), 0)) &&
      !areEqualUpToError(line1.get_b(), line2.get_b())) {
    return false;
  }
  if (!areEqualUpToError(line1.get_b(), 0) && !areEqualUpToError(line2.get_b(), 0) &&
      coef == 0) {
    coef = line1.get_b() / line2.get_b();
  }
  else if (!areEqualUpToError(line1.get_b(), 0) && !areEqualUpToError(line2.get_b(), 0) &&
           !areEqualUpToError(line1.get_b(), line2.get_b() * coef)) {
    return false;
  }
  return areEqualUpToError(line1.get_c(), line2.get_c() * coef);
}


Point Point::projection(const Line& axis) const {
  Point projection(0, 0);
  double a = axis.get_a(), b = axis.get_b(), c = axis.get_c();
  projection.x = (b * b * x - a * b * y - a * c) / (a * a + b * b);
  projection.y = (a * a * y - a * b * x - b * c) / (a * a + b * b);
  return projection;
}

void Point::reflect(const Line& axis) {
  reflect(projection(axis));
}


class Shape {
public:
  virtual ~Shape() = default;

  virtual double perimeter() const = 0;

  virtual double area() const = 0;

  virtual bool isCongruentTo(const Shape& other) const = 0;

  virtual bool isSimilarTo(const Shape& other) const = 0;

  virtual bool containsPoint(const Point& point) const = 0;

  virtual void rotate(const Point& center, double angle) = 0;

  virtual void reflect(const Point& center) = 0;

  virtual void reflect(const Line& axis) = 0;

  virtual void scale(const Point& center, double coefficient) = 0;
  
  virtual bool isEqualTo(const Shape& other) const = 0;
};


bool operator==(const Shape& shape1, const Shape& shape2) {
  return shape1.isEqualTo(shape2);
}


class Ellipse : public Shape {
protected:
  Point focus1, focus2;
  double sum_of_distances;

public:
  Ellipse() : focus1{0, 0}, focus2{0, 0}, sum_of_distances(0) {}

  explicit Ellipse(const Point& focus1, const Point focus2, double sum_of_distances) : focus1(focus1), focus2(focus2),
                                                                                       sum_of_distances(sum_of_distances) {}
                                                                                      
  bool isEqualTo(const Shape& other) const override {
    const Ellipse* check = dynamic_cast<const Ellipse*>(&other);
    if (!check) {
      return false;
    }
    const Ellipse& other_ellipse = *check;
    return areEqualUpToError(sum_of_distances, other_ellipse.sum_of_distances) &&
           ((focus1 == other_ellipse.focus1 && focus2 == other_ellipse.focus2) ||
            (focus1 == other_ellipse.focus2 && focus2 == other_ellipse.focus1));
  }

  std::pair<Point, Point> focuses() const {
    return {focus1, focus2};
  }

  double eccentricity() const {
    return distance_between_points(focus1, focus2) / sum_of_distances;
  }

  Point center() const {
    return (focus1 + focus2) / 2;
  }

  std::pair<Line, Line> directrices() const {
    Point point1 = center() + (focus1 - center()) / eccentricity() / eccentricity();
    Point point2 = point1;
    point2.reflect(center());
    Line fline(focus1, focus2);
    Line directrix1(-fline.get_b(), fline.get_a(), fline.get_b() * point1.x - fline.get_a() * point1.y);
    Line directrix2(-fline.get_b(), fline.get_a(), fline.get_b() * point2.x - fline.get_a() * point2.y);
    return {directrix1, directrix2};
  }

  double perimeter() const override {
    return 2 * sum_of_distances * std::comp_ellint_2(eccentricity());
  }

  double area() const override {
    const int magic_constant = 4;
    double b = sqrt(sum_of_distances * sum_of_distances / magic_constant -
                    distance_between_points(focus1, focus2) *
                    distance_between_points(focus1, focus2) / magic_constant);
    return std::acos(-1) * sum_of_distances / 2 * b;
  }

  bool isCongruentTo(const Shape& other) const override {
    const Ellipse* check = dynamic_cast<const Ellipse*>(&other);
    if (!check) {
      return false;
    }
    const Ellipse& other_ellipse = *check;
    return areEqualUpToError(sum_of_distances, other_ellipse.sum_of_distances) &&
           areEqualUpToError(distance_between_points(focus1, focus2),
                             distance_between_points(other_ellipse.focus1, other_ellipse.focus2));
  }

  bool isSimilarTo(const Shape& other) const override {
    const Ellipse* check = dynamic_cast<const Ellipse*>(&other);
    if (!check) {
      return false;
    }
    const Ellipse& other_ellipse = *check;
    double k = sum_of_distances / other_ellipse.sum_of_distances;
    return areEqualUpToError(distance_between_points(focus1, focus2),
                             distance_between_points(other_ellipse.focus1, other_ellipse.focus2) * k);
  }

  bool containsPoint(const Point& point) const override {
    return distance_between_points(focus1, point) + distance_between_points(focus2, point) <= sum_of_distances;
  }

  void rotate(const Point& center, double angle) override {
    focus1.rotate(center, angle);
    focus2.rotate(center, angle);
  }

  void reflect(const Point& center) override {
    focus1.reflect(center);
    focus2.reflect(center);
  }

  void reflect(const Line& axis) override {
    focus1.reflect(axis);
    focus2.reflect(axis);
  }

  void scale(const Point& center, double coefficient) override {
    focus1.scale(center, coefficient);
    focus2.scale(center, coefficient);
    sum_of_distances *= coefficient;
  }
};


class Circle: public Ellipse {
public:
  explicit Circle(const Point& center, double radius) {
    focus1 = focus2 = center;
    sum_of_distances = 2 * radius;
  }

  double radius() const {
    return sum_of_distances / 2;
  }
};


class Polygon: public Shape {
private:
  void addVertices(const Point& point, const Point& other_points...) {
    vertices.push_back(point);
    addVertices(other_points);
  }

  void addVertices(const Point& point) {
    vertices.push_back(point);
  }

  size_t mod(size_t n) const {
    return n >= vertices.size() ? n - vertices.size() : n;
  }

  bool isCongruentToInTraversalOrder(const Polygon& other, bool reverse=false, bool similar=false) const {
    for (size_t start = 0; start < vertices.size(); ++start) {
      bool congruent = true;
      double angle_coef = 0;
      double coefficient = distance_between_points(vertices[mod(start + 1)], vertices[start]) /
                           (!reverse ? distance_between_points(other.vertices[1], other.vertices[0]) :
                            distance_between_points(other.vertices[vertices.size() - 2], other.vertices.back()));
      for (size_t i = 0; i < vertices.size(); ++i) {
        Point vecta1 = vertices[mod(start + i + 1)] - vertices[mod(start + i)];
        Point vecta2 = vertices[mod(mod(start + i + 2))] - vertices[mod(start + i + 1)];
        Point vectb1 = !reverse ? other.vertices[mod(i + 1)] - other.vertices[i] :
                       other.vertices[mod(2 * vertices.size() - i - 2)] - other.vertices[vertices.size() - 1 - i];
        Point vectb2 = !reverse ? other.vertices[mod(i + 2)] - other.vertices[mod(i + 1)] :
                       other.vertices[mod(2 * vertices.size() - i - 2 - 1)] - other.vertices[mod(2 * vertices.size() - 2 - i)];
        double anglea = std::atan2(vectorProduct(vecta1, vecta2), scalarProduct(vecta1, vecta2));
        double angleb = std::atan2(vectorProduct(vectb1, vectb2), scalarProduct(vectb1, vectb2));
        if (i == 0 && areEqualUpToError(std::abs(anglea), std::abs(angleb))) {
          angle_coef = (anglea > 0) == (angleb > 0) ? 1 : -1;
        }
        else if (i == 0) {
          congruent = false;
          break;
        }
        if (!similar && (!areEqualUpToError(length(vecta1), length(vectb1)) ||
            !areEqualUpToError(anglea, angleb * angle_coef))) {
          congruent = false;
          break;
        }
        if (similar && (!areEqualUpToError(length(vecta1), length(vectb1) * coefficient) ||
            !areEqualUpToError(anglea, angleb * angle_coef))) {
          congruent = false;
          break;
        }
      }
      if (congruent) {
        return true;
      }
    }
    return false;
  }
  
    bool isEqualInTraversalOrder(const Polygon& other, bool reverse= false) const {
    for (size_t start = 0; start < vertices.size(); ++start) {
      bool equal = true;
      for (size_t i = 0; i < vertices.size(); ++i) {
        if (vertices[mod(start + i)] != other.vertices[!reverse ? i : vertices.size() - 1 - i]) {
          equal = false;
          break;
        }
      }
      if (equal) {
        return true;
      }
    }
    return false;
  }


protected:
  std::vector<Point> vertices;

public:
  Polygon() = default;

  explicit Polygon(std::vector<Point>  vertices): vertices(std::move(vertices)) {}

  Polygon(const std::initializer_list<Point>& lst): vertices(lst) {}

  explicit Polygon(const Point& vertices...) {
    addVertices(vertices);
  }
  
  bool isEqualTo(const Shape& other) const override {
    const Polygon* check = dynamic_cast<const Polygon*>(&other);
    if (!check) {
      return false;
    }
    const Polygon& other_polygon = *check;
    if (verticesCount() != other_polygon.verticesCount()) {
      return false;
    }
    return isEqualInTraversalOrder(other_polygon) || isEqualInTraversalOrder(other_polygon, true);
  }

  double perimeter() const override {
    double res = 0;
    for (size_t i = 0; i + 1 < vertices.size(); ++i) {
      res += distance_between_points(vertices[i], vertices[i + 1]);
    }
    res += distance_between_points(vertices.back(), vertices[0]);
    return res;
  }

  double area() const override {
    double res = 0;
    for (size_t i = 1; i + 1 < vertices.size(); ++i) {
      res += vectorProduct(vertices[i] - vertices[0], vertices[i + 1] - vertices[0]);
    }
    return std::abs(res / 2);
  }

  bool isCongruentTo(const Shape& other) const override {
    const Polygon* check = dynamic_cast<const Polygon*>(&other);
    if (!check) {
      return false;
    }
    const Polygon& other_polygon = *check;
    if (vertices.size() != other_polygon.vertices.size()) {
      return false;
    }
    return isCongruentToInTraversalOrder(other_polygon) ||
           isCongruentToInTraversalOrder(other_polygon, true);
  }

  bool isSimilarTo(const Shape& other) const override {
    const Polygon* check = dynamic_cast<const Polygon*>(&other);
    if (!check) {
      return false;
    }
    const Polygon& other_polygon = *check;
    if (vertices.size() != other_polygon.vertices.size()) {
      return false;
    }
    return isCongruentToInTraversalOrder(other_polygon, false, true) ||
           isCongruentToInTraversalOrder(other_polygon, true, true);
  }

  bool containsPoint(const Point& point) const override {
    double angle_sum = 0;
    for (size_t i = 0; i < vertices.size(); ++i) {
      if (point == vertices[i]) {
        return true;
      }
      Point vect1 = vertices[i] - point;
      Point vect2 = vertices[mod(i + 1)] - point;
      double oriented_angle = std::atan2(vectorProduct(vect1, vect2), scalarProduct(vect1, vect2));
      if (areEqualUpToError(vectorProduct(vect1, vect2), 0) && scalarProduct(vect1, vect2) < 0) {
        return true;
      }
      angle_sum += oriented_angle;
    }
    return !areEqualUpToError(angle_sum, 0);
  }

  void rotate(const Point& center, double angle) override {
    for (size_t i = 0; i < vertices.size(); ++i) {
      vertices[i].rotate(center, angle);
    }
  }

  void reflect(const Point& center) override {
    for (size_t i = 0; i < vertices.size(); ++i) {
      vertices[i].reflect(center);
    }
  }

  void reflect(const Line& axis) override {
    for (size_t i = 0; i < vertices.size(); ++i) {
      vertices[i].reflect(axis);
    }
  }

  void scale(const Point& center, double coefficient) override {
    for (size_t i = 0; i < vertices.size(); ++i) {
      vertices[i].scale(center, coefficient);
    }
  }

  bool isConvex() const {
    bool positive = true;
    if (vectorProduct(vertices[1] - vertices[0], vertices[2] - vertices[1]) < 0) {
      positive = false;
    }
    for (size_t i = 1; i < vertices.size(); ++i) {
      if ((vectorProduct(vertices[mod(i + 1)] - vertices[i],
                         vertices[mod(i + 2)] - vertices[mod(i + 1)]) > 0) ^ positive) {
        return false;
      }
    }
    return true;
  }

  size_t verticesCount() const {
    return vertices.size();
  }

  std::vector<Point> getVertices() const {
    return vertices;
  }
};


class Rectangle: public Polygon {
public:
  Rectangle() = default;

  explicit Rectangle(const Point& vertice1, const Point& vertice2, double ratio) {
    if (ratio > 1) {
      ratio = 1 / ratio;
    }
    double angle = std::acos(-1) - 2 * std::atan(ratio);
    Point center = (vertice1 + vertice2) / 2;
    Point new_vertice1 = vertice1;
    new_vertice1.rotate(center, toDegrees(angle));
    Point new_vertice2 = new_vertice1;
    new_vertice2.reflect(center);
    vertices = {vertice1, new_vertice1, vertice2, new_vertice2};
  }

  Point center() const {
    return (vertices[0] + vertices[2]) / 2;
  }
  
  Circle circumscribedCircle() const {
    return Circle(center(), distance_between_points(center(), vertices[0]));
  }

  std::pair<Line, Line> diagonals() const {
    return {Line(vertices[0], vertices[2]), Line(vertices[1], vertices[2 + 1])};
  }
};


class Square: public Rectangle {
public:
  Square() = default;

  explicit Square(const Point& vertice1, const Point& vertice2) {
    const int pi_4 = 45;
    Point new_vertice1 = vertice1 * (1 / std::sqrt(2)) + vertice2 * (1 - 1 / std::sqrt(2));
    new_vertice1.rotate(vertice2, pi_4);
    Point new_vertice2 = vertice1 * (1 - 1 / std::sqrt(2)) + vertice2 * (1 / std::sqrt(2));
    new_vertice2.rotate(vertice1, pi_4);
    vertices = {vertice1, new_vertice1, vertice2, new_vertice2};
  }

  Circle inscribedCircle() const {
    return Circle(center(), distance_between_points(center(), (vertices[0] + vertices[1]) / 2));
  }
};


class Triangle: public Polygon {
public:
  explicit Triangle(const Point& vertice1, const Point& vertice2, const Point& vertice3) {
    vertices = {vertice1, vertice2, vertice3};
  }

  Point orthocenter() const {
    Line height1(vertices[0], vertices[0].projection(Line(vertices[1], vertices[2])));
    Line height2(vertices[1], vertices[1].projection(Line(vertices[0], vertices[2])));
    return height1.intersection(height2);
  }

  Point circumscribedCircleCenter() const {
    return Triangle((vertices[0] + vertices[1]) / 2, (vertices[0] + vertices[2]) / 2,
                    (vertices[1] + vertices[2]) / 2).orthocenter();
  }

  Circle circumscribedCircle() const {
    Point center = circumscribedCircleCenter();
    return Circle(center, distance_between_points(center, vertices[0]));
  }

  Circle inscribedCircle() const {
    double side0 = distance_between_points(vertices[0], vertices[1]);
    double side1 = distance_between_points(vertices[1], vertices[2]);
    double side2 = distance_between_points(vertices[2], vertices[0]);
    Line bisector1(vertices[0],
                   vertices[1] * side2 / (side0 + side2) + vertices[2] * side0 / (side0 + side2));
    Line bisector2(vertices[1],
                   vertices[2] * side0 / (side0 + side1) + vertices[0] * side1 / (side0 + side1));
    Point center = bisector1.intersection(bisector2);
    return Circle(center, distance_between_points(center,
                                                  center.projection(Line(vertices[0], vertices[1]))));
  }

  Point centroid() const {
    const int three = 3;
    return (vertices[0] + vertices[1] + vertices[2]) / three;
  }

  Line EulerLine() const {
    return Line(centroid(), orthocenter());
  }

  Circle ninePointsCircle() const {
    Point center = (orthocenter() + circumscribedCircleCenter()) / 2;
    return Circle(center, distance_between_points(center, (vertices[0] + vertices[1]) / 2));
  }
};
