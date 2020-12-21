
use decent_toml_parser as toml;

#[test]
fn parse_test() {

    let input = r#"
    [[name]]

    [[name.b]]
    [[name.b]]

    [name.b.c]
    hello = 1

    [[name.b]]
    c = 1
    "#;

    let d = toml::from_lines_to_value(input);
    assert!(false, "{:?}", d);
}


/*

    name: [
        {},
        {},
        {}, <-- only remember defintions for that one
    ]
    hello: [
        {},
        {},
        {}, <-- only remember definitions for that one
        {}, <-- then, new table! forget old definitions
    ]


    name {
        a {

        }
    }

    hello {
        b {

        }
        c {

        }
    }

    a {
        b {
            c {

            }
        }
    }
*/


