use crate::location::{Meta, Span};
use std::rc::Rc;

pub fn parse<'s, T, L, E, P>(parser: &P, source: &'s str, location: L) -> ParseResult<'s, T, L, E>
where
    P: Parser<T, L, E>,
{
    parser.parse(source, location)
}

/// A parser takes an input source and produces an array of potential tagged values and an array of
/// errors.
pub trait Parser<T, L, E = ()> {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L, E>;
}

impl<T, L, E, F> Parser<T, L, E> for F
where
    F: for<'s> Fn(&'s str, L) -> ParseResult<'s, T, L, E>,
{
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L, E> {
        self(source, location)
    }
}

#[derive(Debug)]
pub struct ParseResult<'s, T, L, E = ()> {
    start: L,
    success: bool,
    failure: bool,
    results: Vec<InnerResult<'s, T, L, E>>,
}

impl<'s, T, L, E> ParseResult<'s, T, L, E> {
    pub fn none(location: L) -> Self {
        ParseResult {
            start: location,
            success: false,
            failure: false,
            results: vec![],
        }
    }

    pub fn values<'r>(&'r self) -> impl Iterator<Item = &'r Meta<T, L>> + 'r {
        self.results.iter().map(InnerResult::value)
    }

    pub fn is_success(&self) -> bool {
        self.success && !self.failure
    }

    pub fn is_none(&self) -> bool {
        !self.failure && !self.success
    }

    pub fn single_parse(&self) -> bool {
        self.results.len() == 1
    }

    pub fn or(self, other: Self) -> Self {
        let ParseResult {
            start,
            mut success,
            mut failure,
            mut results,
        } = self;
        let ParseResult {
            success: other_success,
            failure: other_failure,
            results: mut other_results,
            ..
        } = other;
        success |= other_success;
        failure |= other_failure;
        results.append(&mut other_results);
        ParseResult {
            start,
            success,
            failure,
            results,
        }
    }

    pub fn map<A, F>(self, map: &F) -> ParseResult<'s, A, L, E>
    where
        F: Fn(T) -> A,
    {
        let ParseResult {
            start,
            success,
            failure,
            results,
        } = self;
        let results = results.into_iter().map(|result| result.map(map)).collect();
        ParseResult {
            start,
            success,
            failure,
            results,
        }
    }

    pub fn catch(self) -> Self {
        let ParseResult {
            start,
            success,
            failure,
            results,
        } = self;
        let results = results.into_iter().map(InnerResult::catch).collect();
        ParseResult {
            start,
            success,
            failure,
            results,
        }
    }
}

impl<'s, T, L: Clone, E> ParseResult<'s, T, L, E> {
    pub fn errors<'r>(&'r self) -> impl Iterator<Item = Meta<impl AsRef<E>, L>> + 'r {
        self.results
            .iter()
            .flat_map(InnerResult::errors)
            .map(|error| error.as_ref().clone())
    }

    pub fn exceptions<'r>(&'r self) -> impl Iterator<Item = Meta<impl AsRef<E>, L>> + 'r {
        self.results
            .iter()
            .flat_map(InnerResult::exceptions)
            .map(|error| error.as_ref().clone())
    }

    pub fn meta(self) -> ParseResult<'s, Meta<T, L>, L, E> {
        let ParseResult {
            start,
            success,
            failure,
            results,
        } = self;
        let results = results.into_iter().map(InnerResult::meta).collect();
        ParseResult {
            start,
            success,
            failure,
            results,
        }
    }
}

impl<'s, T, L: Clone, E: Clone> ParseResult<'s, T, L, E> {
    pub fn cloned_errors<'r>(&'r self) -> impl Iterator<Item = Meta<E, L>> + 'r {
        self.results
            .iter()
            .flat_map(InnerResult::errors)
            .map(|error| error.as_ref().clone().map(|error| error.as_ref().clone()))
    }

    pub fn cloned_exceptions<'r>(&'r self) -> impl Iterator<Item = Meta<E, L>> + 'r {
        self.results
            .iter()
            .flat_map(InnerResult::exceptions)
            .map(|error| error.as_ref().clone().map(|error| error.as_ref().clone()))
    }
}

impl<'s, T, L: Span, E> ParseResult<'s, T, L, E> {
    pub fn error(value: T, error: E, source: &'s str, location: L) -> Self {
        ParseResult {
            start: location.clone(),
            success: false,
            failure: true,
            results: vec![InnerResult::failure(value, error, source, location)],
        }
    }

    pub fn exception(value: T, error: E, source: &'s str, location: L) -> Self {
        ParseResult {
            start: location.clone(),
            success: false,
            failure: true,
            results: vec![InnerResult::exception(value, error, source, location)],
        }
    }

    pub fn success(value: T, source: &'s str, location: L) -> Self {
        ParseResult {
            start: location.clone(),
            success: true,
            failure: false,
            results: vec![InnerResult::success(value, source, location)],
        }
    }

    pub fn and_then<A, F>(self, next: &F) -> ParseResult<'s, A, L, E>
    where
        F: Fn(T, &'s str, L) -> ParseResult<'s, A, L, E>,
    {
        let ParseResult { start, results, .. } = self;
        let mut new_results = vec![];
        let mut success = false;
        let mut failure = false;
        let iter = results.into_iter().map(|result| result.and_then(next));
        for ParseResult {
            mut results,
            success: new_success,
            failure: new_failure,
            ..
        } in iter
        {
            success |= new_success;
            failure |= new_failure;
            new_results.append(&mut results);
        }
        ParseResult {
            start,
            success,
            failure,
            results: new_results,
        }
    }
}

#[derive(Debug)]
struct InnerResult<'s, T, L, E = ()> {
    value: Meta<T, L>,
    exceptions: List<Meta<Rc<E>, L>>,
    errors: List<Meta<Rc<E>, L>>,
    source: &'s str,
    location: L,
}

impl<'s, T, L, E> InnerResult<'s, T, L, E> {
    fn value(&self) -> &Meta<T, L> {
        &self.value
    }

    fn exceptions(&self) -> List<Meta<Rc<E>, L>> {
        self.exceptions.reverse()
    }

    fn errors(&self) -> List<Meta<Rc<E>, L>> {
        self.errors.reverse()
    }

    fn is_failure(&self) -> bool {
        self.exceptions.length() > 0 || self.errors.length() > 0
    }

    fn map<A, F: Fn(T) -> A>(self, map: F) -> InnerResult<'s, A, L, E> {
        let InnerResult {
            value,
            exceptions,
            errors,
            source,
            location,
        } = self;
        let value = value.map(map);
        InnerResult {
            value,
            exceptions,
            errors,
            source,
            location,
        }
    }

    fn catch(self) -> Self {
        let InnerResult {
            value,
            exceptions,
            errors,
            source,
            location,
        } = self;
        let errors = errors.concat(&exceptions);
        let exceptions = List::new();
        InnerResult {
            value,
            exceptions,
            errors,
            source,
            location,
        }
    }
}

impl<'s, T, L: Clone, E> InnerResult<'s, T, L, E> {
    fn meta(self) -> InnerResult<'s, Meta<T, L>, L, E> {
        let InnerResult {
            value,
            exceptions,
            errors,
            source,
            location,
        } = self;
        let value = Meta::new(value, location.clone());
        InnerResult {
            value,
            exceptions,
            errors,
            source,
            location,
        }
    }
}

impl<'s, T, L: Span, E> InnerResult<'s, T, L, E> {
    fn success(value: T, source: &'s str, mut location: L) -> Self {
        let value = Meta::new(value, location.take());
        InnerResult {
            value,
            exceptions: List::new(),
            errors: List::new(),
            source,
            location,
        }
    }

    fn failure(value: T, error: E, source: &'s str, mut location: L) -> Self {
        let parse_location = location.take();
        let value = Meta::new(value, parse_location.clone());
        let error = Meta::new(Rc::new(error), parse_location);
        InnerResult {
            value,
            exceptions: List::new(),
            errors: List::single(error),
            source,
            location,
        }
    }

    fn exception(value: T, exception: E, source: &'s str, mut location: L) -> Self {
        let parse_location = location.take();
        let value = Meta::new(value, parse_location.clone());
        let exception = Meta::new(Rc::new(exception), parse_location);
        InnerResult {
            value,
            exceptions: List::single(exception),
            errors: List::new(),
            source,
            location,
        }
    }

    fn and_then<A, F>(self, next: &F) -> ParseResult<'s, A, L, E>
    where
        F: Fn(T, &'s str, L) -> ParseResult<'s, A, L, E>,
    {
        let InnerResult {
            value,
            exceptions,
            errors,
            source,
            location,
        } = self;
        // Take out the current value
        let (value, start) = value.split();
        // Get the results of parsing after the current value
        let result = next(value, source, location.clone());
        let ParseResult {
            success,
            mut failure,
            results,
            ..
        } = result;

        // Process the results
        let results: Vec<_> = results
            .into_iter()
            .map(|result| result.combine(&start, exceptions.clone(), errors.clone()))
            .collect();

        failure |= results.iter().any(InnerResult::is_failure);

        ParseResult {
            start,
            success,
            failure,
            results,
        }
    }

    fn combine(
        self,
        start: &L,
        existing_exceptions: List<Meta<Rc<E>, L>>,
        existing_errors: List<Meta<Rc<E>, L>>,
    ) -> Self {
        let InnerResult {
            value,
            exceptions,
            errors,
            source,
            location,
        } = self;

        // Adjust the location of the value
        let (value, mut value_location) = value.split();
        value_location.move_start(start.start().clone());
        let value = Meta::new(value, value_location);

        // Move the start of all the uncaught exceptions
        let exceptions: List<_> = exceptions
            .concat(&existing_exceptions)
            .map(|meta| {
                let (exception, mut location) = meta.as_ref().clone().split();
                location.move_start(start.start().clone());
                Meta::new(exception, location)
            })
            .collect();

        // Concatenate all of the errors
        let errors = existing_errors.concat(&errors);

        InnerResult {
            value,
            exceptions,
            errors,
            source,
            location,
        }
    }
}

/// Persistent linked-list data structure
#[derive(Debug)]
pub struct List<T>(Rc<Node<T>>, usize);

impl<T> List<T> {
    /// Create a new, empty list
    pub fn new() -> Self {
        List(Rc::new(Node::Tail), 0)
    }

    pub fn single(value: T) -> Self {
        Self::new().push(value)
    }

    pub fn length(&self) -> usize {
        self.1
    }

    /// Push an item onto the list
    pub fn push(&self, value: T) -> Self {
        let List(node, length) = self;
        let node = Node::Node(Rc::new(value), node.clone());
        List(Rc::new(node), *length + 1)
    }

    pub fn push_rc(&self, value: Rc<T>) -> Self {
        let List(node, length) = self;
        let node = Node::Node(value, node.clone());
        List(Rc::new(node), *length + 1)
    }

    /// Create the reversed copy of the list
    pub fn reverse(&self) -> Self {
        let mut reversed = List::new();
        for node in self.clone() {
            reversed = reversed.push_rc(node);
        }
        return reversed;
    }

    /// Concatenate two lists
    pub fn concat(&self, other: &Self) -> Self {
        let mut joined = self.clone();
        for node in other.reverse() {
            joined = joined.push_rc(node);
        }
        joined
    }
}

impl<T> Clone for List<T> {
    fn clone(&self) -> Self {
        List(self.0.clone(), self.1)
    }
}

impl ToString for List<char> {
    fn to_string(&self) -> String {
        let mut string = String::with_capacity(self.1);
        for character in self.reverse() {
            string.push(*character);
        }
        string
    }
}

impl<T> Iterator for List<T> {
    type Item = Rc<T>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((next, tail)) = self.0.pair() {
            *self = List(tail, self.1 - 1);
            Some(next)
        } else {
            None
        }
    }
}

impl<T> ::std::iter::FromIterator<Rc<T>> for List<T> {
    fn from_iter<I: IntoIterator<Item = Rc<T>>>(iter: I) -> Self {
        let mut list = List::new();
        for item in iter {
            list = list.push_rc(item);
        }
        list
    }
}

impl<T> ::std::iter::FromIterator<T> for List<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut list = List::new();
        for item in iter {
            list = list.push(item);
        }
        list
    }
}

#[derive(Debug)]
enum Node<T> {
    Node(Rc<T>, Rc<Node<T>>),
    Tail,
}

impl<T> Node<T> {
    fn pair(&self) -> Option<(Rc<T>, Rc<Node<T>>)> {
        match self {
            Node::Node(value, next) => Some((value.clone(), next.clone())),
            Node::Tail => None,
        }
    }
}

impl<T> Clone for Node<T> {
    fn clone(&self) -> Self {
        match self {
            Node::Node(value, next) => Node::Node(value.clone(), next.clone()),
            Node::Tail => Node::Tail,
        }
    }
}
