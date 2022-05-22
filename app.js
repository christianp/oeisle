import entry_data from './entries.mjs';
import show_error from './show-error.mjs';

function gregorian_to_julian_day_number(time) {
    const Y = time.getUTCFullYear();
    const M = time.getUTCMonth() + 1;
    const D = time.getUTCDate();
    return Math.trunc((1461 * (Y + 4800 + Math.trunc((M - 14)/12)))/4) + Math.trunc((367 * (M - 2 - 12 * (Math.trunc((M - 14)/12))))/12) - Math.trunc((3 * (Math.trunc((Y + 4900 + Math.trunc((M - 14)/12))/100)))/4) + D - 32075
}
window.gregorian_to_julian_day_number = gregorian_to_julian_day_number;

const first_day = gregorian_to_julian_day_number(new Date(2022,4,22));

async function init_app() {
    const compilation_error = await show_error;
    if(compilation_error) {
        return;
    }
    const app = Elm.OeisleGame.init({node: document.body, flags: {}});

    function load_entry(entry) {
        const [number,name,...seq] = entry;
        const data = {number,name,seq};
        return data;
    }


    const entries = entry_data.map(load_entry);
    window.entries = entries;

    function smallest_factor(t) {
        for(let i=2;i<entries.length;i++) {
            if(t%i==0) {
                return i;
            }
        }
    }

    const t = entries.length;
    const biggest_factor = t/smallest_factor(t);
    const m = t - biggest_factor + 1;

    function nth(n) {
        return (m*n)%t;
    }

    function respond(reason,data) {
        const o = {reason};
        Object.assign(o,data);
        app.ports.receiveEntry.send(o);
    }

    app.ports.loadEntry.subscribe(async message => {
        const [reason,query,random] = message;
        switch(reason) {
            case 'target':
                let n, edition;
                if(random) {
                    n = Math.floor(Math.random()*entries.length);
                    edition = null;
                } else {
                    const now = new Date();
                    const day_number = gregorian_to_julian_day_number(now);
                    n = nth(day_number);
                    edition = day_number - first_day;
                }
                respond(reason,{entry: entries[n], edition: edition});
                break;
            case 'query':
                const matches = entries.filter(e => {
                    return e.seq.slice(0,query.length).every((n,i)=>n==query[i]);
                });
                respond(reason,{matches});
                break;
        }
    });
}

init_app();
