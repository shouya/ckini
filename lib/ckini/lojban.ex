defmodule Ckini.Lojban do
  @moduledoc false

  use Ckini

  @ma Var.new(:ma)

  def ma, do: @ma

  def lo(selbri) do
    v = Var.new()
    selbri.(v, ziho())
    v
  end

  def se(selbri) do
    fn fa, fe ->
      selbri.(fe, fa)
    end
  end

  def cu(fa, selbri, fe) do
    selbri.(fa, fe)
  end

  def cu(fa, selbri) do
    cu(fa, selbri, ziho())
  end

  def be(selbri, sumti) do
    fn fa, _fe ->
      selbri.(fa, sumti)
    end
  end

  def ziho, do: Var.new()

  def nu(bridi) do
    fn _fa, _fe ->
      bridi.()
    end
  end

  def ka(bridi) do
    fn fa, _fe ->
      bridi.(fa)
    end
  end

  def to_brivla(_valsi) do
    fn _fa, _fe ->
      nil
      # this part needs more work
      # eq([valsi, fa, fe])
    end
  end

  def playground do
    mlatu = to_brivla(:mlatu)
    ladru = to_brivla(:ladru)
    pinxe = to_brivla(:pinxe)

    run(@ma, [
      cu(lo(mlatu), pinxe, lo(ladru)),
      cu(@ma, se(pinxe), lo(mlatu))
    ])
  end
end
