
use decent_toml_parser as toml;

#[test]
fn parse_test() {

    let input = r#"
    enjoy = [2, 3]

    [name]
    lol = 89

    [[name.b]]
    hello.world = 8
    [name.b.hello.bye.k]
    l = 9
    [[name.b]]
    "action!.89" = { mmy = 6 }
    
    [[hello]]
    a = true
    [[hello]]
    b = false
   "#;

    let lines = toml::parse_toml_lines(input).unwrap();
    let toml_value = toml::toml_value_from_lines(lines).unwrap();

    assert!(false, "{}", toml::print(&toml_value));
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


