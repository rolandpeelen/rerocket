#[allow(dead_code, unused_imports)]
extern crate proc_macro;
use proc_macro::TokenStream as InternalTokenStream;
use proc_macro2::{Punct, TokenStream, TokenTree};
use quote::quote;
use syn::parse::Parser;
use syn::punctuated::Punctuated;
use syn::{parse_macro_input, Attribute, Ident, ItemFn, Lit, Meta, MetaList};

#[derive(Debug)]
enum DataNameError {
    NoComma,
    NoDataIdentifier,
    NoEquals,
    NoDataNameLiteral,
}

#[derive(Debug)]
enum ParseDataName {
    Keep(String),
    ThrowAway,
}

#[derive(Debug)]
enum ParseError {
    NoRocketAnnotation,
    NoPath,
    DataNameError(DataNameError),
    NoAttributes,
}

#[derive(Debug)]
enum HttpMethod {
    Get,
    Post,
    Put,
    Patch,
    Delete,
}

#[derive(Debug)]
enum HttpMethodParseError {
    InvalidHttpMethod,
}

impl HttpMethod {
    fn from_ident(ident: &Ident) -> Result<Self, HttpMethodParseError> {
        match ident.to_string().as_str() {
            "get" => Ok(Self::Get),
            "post" => Ok(Self::Post),
            "put" => Ok(Self::Put),
            "patch" => Ok(Self::Patch),
            "delete" => Ok(Self::Delete),
            _ => Err(HttpMethodParseError::InvalidHttpMethod),
        }
    }
}

#[derive(Debug)]
struct Route {
    method: HttpMethod,
    path: String,
    data_name: Option<String>,
}

impl Route {
    fn parse_path(input: TokenTree) -> Option<String> {
        match input {
            TokenTree::Literal(str) => Some(str.to_string()),
            _ => None,
        }
    }

    // Punct {
    //     ch: ',',
    //     spacing: Alone,
    //     span: #0 bytes(594..595),
    // },
    // Ident {
    //     ident: "data",
    //     span: #0 bytes(596..600),
    // },
    // Punct {
    //     ch: '=',
    //     spacing: Alone,
    //     span: #0 bytes(601..602),
    // },
    // Literal {
    //     kind: Str,
    //     symbol: "<request>",
    //     suffix: None,
    //     span: #0 bytes(603..614),
    // },
    //

    fn parse_punct(
        token_tree: TokenTree,
        char: char,
        e: DataNameError,
    ) -> Result<ParseDataName, DataNameError> {
        match token_tree {
            TokenTree::Punct(punct) if punct.as_char() == char => Ok(ParseDataName::ThrowAway),
            _ => Err(e),
        }
    }

    fn parse_comma(token_tree: TokenTree) -> Result<ParseDataName, DataNameError> {
        Self::parse_punct(token_tree, ',', DataNameError::NoComma)
    }

    fn parse_equals(token_tree: TokenTree) -> Result<ParseDataName, DataNameError> {
        Self::parse_punct(token_tree, '=', DataNameError::NoEquals)
    }

    fn parse_data_ident(token_tree: TokenTree) -> Result<ParseDataName, DataNameError> {
        match token_tree {
            TokenTree::Ident(ident) if ident.to_string().as_str() == "data" => {
                Ok(ParseDataName::ThrowAway)
            }
            _ => Err(DataNameError::NoDataIdentifier),
        }
    }

    fn parse_data_name(token_tree: TokenTree) -> Result<ParseDataName, DataNameError> {
        match token_tree {
            TokenTree::Literal(literal) => Ok(ParseDataName::Keep(literal.to_string())),
            _ => Err(DataNameError::NoDataNameLiteral),
        }
    }

    fn parse_rest(input: TokenStream) -> Option<Result<String, DataNameError>> {
        let stream = [
            Self::parse_comma,
            Self::parse_data_ident,
            Self::parse_equals,
            Self::parse_data_name,
        ]
        .iter()
        .zip(input.into_iter())
        .map(|(parser, token)| parser(token));

        match stream {
            xs if (xs.size_hint() == (0, None)) => None,
            mut xs => {
                if xs.clone().all(|x| x.is_ok()) {
                    xs.last().and_then(|item| match item {
                        Ok(ParseDataName::Keep(s)) => Some(Ok(s)),
                        _ => None,
                    })
                } else {
                    // Return the first error
                    xs.find(|x| x.is_err()).and_then(|item| match item {
                        Ok(ParseDataName::Keep(s)) => Some(Ok(s)),
                        _ => None,
                    })
                }
            }
        }
    }

    fn parse(method: HttpMethod, input: Attribute) -> Result<Route, ParseError> {
        // The first element in the token stream should always be a literal, holding the name of
        // the route.
        // Path, Delim, Tokens
        // let name = parse_name(input.);

        match input.meta {
            Meta::List(MetaList { tokens, .. }) => {
                let mut iter = tokens.into_iter();
                let path = iter.next().and_then(Self::parse_path);

                let rest = iter.collect::<TokenStream>();
                let data_name = if rest.is_empty() {
                    None
                } else {
                    Self::parse_rest(rest)
                };

                match (method, path, data_name) {
                    (_, None, _) => Err(ParseError::NoPath),
                    (_, _, Some(Err(e))) => Err(ParseError::DataNameError(e)),
                    (method, Some(path), None) => Ok(Route {
                        method,
                        path,
                        data_name: None,
                    }),
                    (method, Some(path), Some(Ok(data_name))) => Ok(Route {
                        method,
                        path,
                        data_name: Some(data_name),
                    }),
                }
            }
            _ => Err(ParseError::NoAttributes),
        }
    }
}

#[proc_macro_attribute]
pub fn re_rocket(_attr: InternalTokenStream, input: InternalTokenStream) -> InternalTokenStream {
    // returing a simple TokenStream for Struct
    let rocket_macro = parse_macro_input!(input as ItemFn);

    let rocket: Result<Route, ParseError> = rocket_macro
        .attrs
        .into_iter()
        .filter_map(
            |attr| match attr.path().get_ident().map(HttpMethod::from_ident) {
                Some(Ok(method)) => Some(Route::parse(method, attr)),
                Some(Err(_)) | None => None,
            },
        )
        .next()
        .unwrap_or(Err(ParseError::NoRocketAnnotation));

    println!("------------------------------------");
    dbg!(rocket);
    println!("------------------------------------");

    InternalTokenStream::from(quote! {})
}
