import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl
import matplotlib.colors as mcolors

mpl.use('TkAgg')

labels = ["cpp","pure","selectcase","bit","cctype"]

funcs = ['is_control','is_printable','is_white','is_blank','is_graphical',
         'is_punctuation','is_alphanum','is_alpha','is_upper','is_lower',
         'is_digit','is_hex_digit']  

def main():
    rdict = {}

    for r in labels:
        fname = "./results/results_{}.txt".format(r)
        res = np.loadtxt(fname)
        res[:,1:] *= 1.e-9
        rdict[r] = res

    print(rdict["cpp"])
    print(rdict["cpp"])

    x = 1.5*np.arange(len(funcs))
    width = 0.2

    fig, ax = plt.subplots(figsize=(10,5))

    rects1 = ax.bar(x-2*width,rdict["cpp"][4,1:],width,label="C++")
    rects2 = ax.bar(x-width,rdict["pure"][4,1:],width,label="fortran_ascii_pure")
    rects2 = ax.bar(x,rdict["selectcase"][4,1:],width,label="fortran_ascii_selectcase")
    rects2 = ax.bar(x+width,rdict["bit"][4,1:],width,label="fortran_ascii_bit")
    rects2 = ax.bar(x+2*width,rdict["cctype"][4,1:],width,label="fortran_ascii_cctype")

    # Add some text for labels, title and custom x-axis tick labels, etc.
    ax.set_ylabel('Billion Characters per Second')
    ax.set_xticks(x)
    ax.set_xticklabels(funcs,rotation=45)
    ax.legend(bbox_to_anchor=(1.02, 1), loc='upper left', borderaxespad=0.)

    fig.tight_layout()
    fig.savefig("speed_vs_method.png",dpi=300)

    cmap = plt.cm.cool
    norm = mcolors.Normalize(vmin=1, vmax=6)

    fig2, ax2 = plt.subplots(figsize=(8,5))

    tects1 = ax2.bar(x-5*width/2,rdict["pure"][0,1:],width,color=cmap(norm(1)),label="1000")
    tects2 = ax2.bar(x-3*width/2,rdict["pure"][1,1:],width,color=cmap(norm(2)),label="10000")
    tects2 = ax2.bar(x-width/2,rdict["pure"][2,1:],width,color=cmap(norm(3)),label="100000")
    tects2 = ax2.bar(x+width/2,rdict["pure"][3,1:],width,color=cmap(norm(4)),label="1000000")
    tects2 = ax2.bar(x+3*width/2,rdict["pure"][4,1:],width,color=cmap(norm(5)),label="10000000")
    tects2 = ax2.bar(x+5*width/2,rdict["pure"][5,1:],width,color=cmap(norm(6)),label="100000000")

    # Add some text for labels, title and custom x-axis tick labels, etc.
    ax2.set_ylabel('Billion Characters per Second')
    ax2.set_xticks(x)
    ax2.set_xticklabels(funcs,rotation=45)
    ax2.set_title("fortran_ascii_pure")
    ax2.legend(bbox_to_anchor=(1.02, 1), loc='upper left', borderaxespad=0.)
    fig2.tight_layout()

    fig2.savefig("speed_vs_nchar.png",dpi=300)
    plt.show()

if __name__ == '__main__':
    main()