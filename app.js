import entries from './entries.mjs';
window.entries = entries;

const app = Elm.OeisleGame.init({node: document.querySelector('main'), flags: {}});


function send_entry(entry) {
    const [number,name,...seq] = entry;
    const data = {number,name,seq};
    console.log(data);
    app.ports.receiveEntry.send(data);
}

app.ports.loadEntry.subscribe(async message => {
    const i = Math.floor(Math.random()*entries.length);
    send_entry(entries[i]);
});
