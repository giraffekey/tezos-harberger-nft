# tezos-harberger-nft

Research project on implementing a Harberger tax for FA2-compliant non-fungible token contracts on the Tezos blockchain.

**WARNING!** This project is experimental and has not been audited for security or bugs. If you are serious about deploying NFTs on Tezos use [Objkt](https://objkt.com/) or [Kalamint](https://kalamint.io/). Otherwise, feel free to take the risk and let me know if you have any problems!

## What is Harberger taxation?

Based on the ideas of economists [Henry George](https://en.wikipedia.org/wiki/Henry_George), [Arnold Harberger](https://en.wikipedia.org/wiki/Arnold_Harberger) and [Glen Weyl](https://glenweyl.com/), Harberger taxation is designed to reduce monopoly rents and inefficient speculative behavior. Although originally devised for more equitable and efficient real-world economies, the concept also has utility within the blockchain space.

### Harberger NFTs

Harberger NFTs are digital tokens that always have an open sell order at a self-assessed price. The token is in constant auction and a sale can be forced at any moment. Holders set the price of their own NFT stock and pay a weekly or monthly tax to the minter on it. High self-assessed values have a low likelihood of a forced sale, but a high periodic tax. Low self-assessed values have a low periodic tax, but a high likelihood of a forced sale.

Holders must deposit cryptocurrency to pay for their tax to maintain ownership over their tokens. This deposit can be withdrawn at any moment. If their deposit ever runs out, a buyer can force a sale at no expense, transferring token ownership to themselves for free. Taxes are lazily netted and can be claimed from the contract by their recipients.

Harberger tax systems result in accurately priced tokens due to this trade-off as well as a potential source of regular income for the artists minting them.

### Example

Let's say a digital artist mints 5 NFTs for their visual artwork. Each is priced at $10 with a 2% weekly tax. They will not need to pay a tax to themselves to maintain ownership. Anyone can force a sale to declare ownership, sending $10 to the artist. They must declare a new price greater than the price they bought at. Let's say they set it to $20. They must now fund their deposit with $0.40/week or they will risk the token being taken from them without payment.

They have 25 weeks to find a buyer for their NFT and make a profit. If they sell 3 weeks after buying, they will make a net profit of `20 - 10 - 0.4 * 3 =` $8.80. If the token is sold at 25 weeks the net profit will be $0. If they cannot find a buyer within this timespan and still wish to own the token, they will need to either increase the price of the token, thus increasing the tax they must pay on it, or continue funding their tax deposit as a weekly pledge to the artist.

Let's say they wish to still profit, so they choose the former. After 25 weeks, they increase the price from $20 to $30. Their tax is now $0.60/week and they have 16 weeks until their potential net profit reaches zero. After 16 weeks, they may increase the price to $100, paying a $2/week tax. They have now spent $30 total on the token ($10 to buy and $20 on tax) and have 35 weeks until their expenses reach $100.

### Further readings

[What is Harberger Tax & Where Does The Blockchain Fit In? - Simon de la Rouviere](https://medium.com/@simondlr/what-is-harberger-tax-where-does-the-blockchain-fit-in-1329046922c6)

[On Radical Markets - Vitalik Buterin](https://vitalik.ca/general/2018/04/20/radical_markets.html)

[Partial Common Ownership - RadicalxChange](https://www.radicalxchange.org/concepts/partial-common-ownership/)

## Usage

### Compile

```bash
ligo compile contract Harberger.mligo --entry-point main
```

### Test

```bash
ligo run test Harberger.mligo
```

### Example storage

```text
{
  ledger = Big_map.literal [
    (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), Set.add 0n (Set.empty : nat set));
  ];
  operators = (Big_map.empty : operator_storage);
  token_metadata = Big_map.literal [
    (0n, {
      name = "My NFT";
      tax = 500n;
      tax_interval = 604800n;
      tax_recipient = ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address);
      extras = (Map.empty : (string, string) map);
    })
  ];
  token_prices = Big_map.literal [
    ((("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), 0n), {current = 7tez; minimum = 7tez});
  ];
  tax_deposits = Big_map.literal [
    (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), 20tez);
  ];
  tax_records = Big_map.literal [
    ((("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), 0n), [
      { value = 7tez;
        start_date = Tezos.now;
        end_date = (None : timestamp option) }
    ]);
  ];
  tax_claims = (Big_map.empty : tax_claim_storage);
}
```
