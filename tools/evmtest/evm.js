const HDWalletProvider = require('@truffle/hdwallet-provider');
const Web3 = require('web3');
const fs = require('fs');

//const serv_addr = 'https://dev-qa02.dev.findora.org:8545';
const serv_addr = 'http://127.0.0.1:8545';

// 定义钱包地址
const account = [
    '0x292F65000a1F09cC0f2dcdE656674E7De1fBeEa7',
    '0x20bEf686d82f567dfc6ECc803732Ef99D9A19FEb',
    '0x4C0a8a149e92D368F14aFB286BC1e0ecEaa4AaFe',
    '0x3e930Ef583c3E7940ABDA49d7086302Ce703A071',
    '0x5b9B9a20f1820487b4247F001aBCfa6500C7Ae37',
    '0x96521d1b8AEeB261E264e6E76eaCd11D170f2CdC',
    '0x5a76E8623d95486DA16b40B36479571756D7Cd00',
    '0xbD1F881C7a5f3177a4e3bBb86557C26A484389A2',
    '0x272B7509592E601f26b3a97f6416Af287E042C4e',
    '0xc48726998C5ec4DD00D13b2050CF02CAaF7726eb',
    '0x30eEacFC5C2549C14F0974eD33223D257aCc6f14',
    '0xc5De2952bE452034329c37c885eBE379377b0495',
    '0xDd2D2742d205576c96a042D6033bf0C22bb47b9c',
    '0xC5044899eF1D477d60ea7F294AA7344B1116e0fD',
    '0xD3c3f99bC6148541B7E6dD533773fe59c5Fc7148',
    '0x0be7769068838A48553bB08D79015c32eA733445',
    '0xBe23b14F3f8470f59E959665d4Ec537316bf2054',
    '0xD92341af5cEE8E956C3c6e2df93A87d0F4469E2D',
    '0xa600049c69305E0A389E53ac57e0933863252865',
    '0xd8513228409e4174562eFFf3B0AD6344d336177F'
];

// 定义钱包私钥
const pk = [
    'c6815044c5da5572b44560245083cd8349dde4da2591e78d743b506df841e8a1',
    '53dc20772bc99bda6773e432ab0bdaa7302a0d89f6e575b2f73d4b9993351198',
    'd1615f1a9dd3f95d630dfe308d0ceabda1c1bb28bedba43bb77e09262a74fc24',
    '90f299c64b9bc2d9ae0de2e86707eb73ffc99595b1bbffd5fe58c3ffa2b7e291',
    '0fee9b8ab90194d60fd332eb2627563da797e27d6917403b05c31e1cf6a63a24',
    '63b23c40cbc244da81eb44e79120b6432090a1916f5cb8e11e9e970aec622031',
    '3f5e90883771c76fb4e17d4843b8cdfe951d1b7ef2c3b4682ac2da311c2659dd',
    '373dfc341d8491b627754ff3ff3ec760b8bddf854dfc36d4e25f3c1a789b19b8',
    'acb47b075c2437f4b67f48aebaf84c1f02bad6a197a1b6eb4384d779a1fbcf36',
    '6e12391979312d9015e868238ca866200d4b91a8143940f93d6b39d8e7df36e4',
    'f4bafaa6de025816003b89677a853bc95dc550c42f0e7c0840ee8e1b01f05175',
    '5f02aeeaff10aa6d25e362134c47c3532127fa25a403658080e3ec20e11f8687',
    'ee8b5e8234bc5bc26425e34f06702407a241745b5711673fb67ba0d86c5a9598',
    '1aa3cba091d118b166898f6e4cfcc1a54e7197c62eafeec46af20c8be0133bd0',
    'da8e148ce96332ca4fc745e177159bb216e0659b464517bf7745a29a33e61374',
    '407c92a7ddfbd58a7e990471b712510d14b4f51c85600031716ab5d794b94b4b',
    'ec1dd8c17a173f54f3d0a5dd4e050b836a64cab9a725f31e676ff206f07acc16',
    'a52780add50531845d0697007d0a7824c5e8aae410ab894142c019f7dd43301a',
    '71c8df9e2cadf7189f84853face40c263b4cfdd7b96fba710b1aa6ed6a9d4d6e',
    'c197a87775d721e2ebe3515c8d85ca98158ad87d76cce5beb3c5d72f558447b2'
];

// 延时
function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

async function initfra() {
    const provider = new HDWalletProvider(
        pk,
        serv_addr,
    );
    const web3 = new Web3(provider);

    let i = 0;
    const data = fs.readFileSync('address', 'UTF-8');
    // split the contents by new line
    const lines = data.split(/\r?\n/);

    let promises = [];

    // 并发数不要超过accounts数量
    const concurrence = 20;
    try {
        for (let j = 0; j < lines.length; j++) {

            if (i === 20) {
                i = 0;
            }

            const txPromise = new Promise((resolve, reject) => {
                const caller = account[i];
                const receiver = lines[j];
                if (receiver === '' || receiver === undefined) {
                    reject('invalid receiver');
                }
                const idx = j;
                web3.eth.getTransactionCount(caller).then((nonce) => {
                    console.log('caller: ' + caller, 'nonce：' + nonce);
                    web3.eth.sendTransaction({
                        from: caller,
                        to: receiver,
                        value: web3.utils.toWei('200'),
                        gas: web3.utils.toHex(8000000),
                        nonce: web3.utils.toHex(nonce),
                    }).then((res) => {
                        console.log('第' + idx + '个钱包：' + receiver + '完成转账');
                        resolve(res);
                    }, (err2) => {
                        console.log('第' + idx + '个钱包：' + receiver + '转账失败: ', err2);
                        reject('transfer failed');
                    });
                }, (err) => {
                    console.log('query nonce failed: ' + err);
                    reject('query nonce failed');
                });
            });

            promises.push(txPromise);

            await sleep(10 * 1000)

            i++;

            if (promises.length === concurrence) {
                await Promise.all(promises).then(values => {
                    console.log(values);
                });
                promises = [];
            }

        }
    } catch (error) {
        console.log(error);
    }
    process.exit();
}

initfra();
