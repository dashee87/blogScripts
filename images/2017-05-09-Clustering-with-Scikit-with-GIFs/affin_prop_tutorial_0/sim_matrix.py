from sklearn.metrics import silhouette_score,euclidean_distances
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import matplotlib.patches as patches
img_count=[0]

def fig_caption(text, img_count):
    t=plt.figtext(0.5, 0.035, text, horizontalalignment='center',
            fontsize=13, multialignment='left',
            bbox=dict(boxstyle="round", facecolor='#D8D8D8',
                      ec="0.5", pad=0.5, alpha=1), fontweight='bold')
    plt.savefig('C:/Users/Terri/Desktop/Blog/blog 6/affin_prop/tutorial/sim_'+str(len(img_count))+'.png')
    plt.gcf().texts.remove(t)
    img_count.append(0)

hier_data = np.array([[10,10],[15,10],[10,15],[5,10],[10,5],
                      [12.5,10],[10,12.5],[7.5,10],[10,7.5],
                      [12.5,7.5],[12.5,12.5],[7.5,7.5],[7.5,12.5]])

#plt.scatter(hier_data[:,0],hier_data[:,1])
#plt.show()

sim_matrix = -euclidean_distances(hier_data, squared=True)
f, (ax2,ax1) = plt.subplots(1,2,figsize=(10, 5))

# Generate a custom diverging colormap
cmap = sns.diverging_palette(220, 10, as_cmap=True)

# Draw the heatmap with the mask and correct aspect ratio
sns.heatmap(sim_matrix, vmax=0,cmap="YlGnBu_r",
            square=True, xticklabels=False, yticklabels=False,
            linewidths=.5, cbar_kws={"shrink": .5}, ax=ax1)
ax2.scatter(hier_data[:,0],hier_data[:,1],s=50)
plt.tight_layout()
plt.subplots_adjust(bottom=0.135)
plt.savefig('C:/Users/Terri/Desktop/Blog/blog 6/affin_prop/tutorial/sim_0.png')
fig_caption(text="The heatmap on the right represents a similarity matrix (S)", img_count=img_count)

rect=ax1.add_patch(patches.Rectangle((0, len(hier_data)-1),len(hier_data),1,fill=False,lw=1.5))
circ0=ax2.add_artist(plt.Circle((10, 10), 0.2, color='black'))
fig_caption(text="The first row of the matrix indicates the similarity...", img_count=img_count)
fig_caption(text=" between the first point and the other points in the set", img_count=img_count)
arrows=[]
rects=[]
for i in range(1,len(hier_data)):
    arrows.append(ax2.arrow(hier_data[0,0], hier_data[0,1], hier_data[i,0]-hier_data[0,0], hier_data[i,1]-hier_data[0,1],
               head_width=0.5, head_length=0.5, fc='k', ec='k',length_includes_head=True))
    rects.append(ax1.add_patch(patches.Rectangle((i, len(hier_data)-1),1,1,fill=False,lw=1.5)))
fig_caption(text="A typical measure of similarity is negative squared Euclidean distance", img_count=img_count)
fig_caption(text="A typical measure of similarity is negative squared Euclidean distance", img_count=img_count)
rect.remove()
circ0.remove()
for i in range(0,len(hier_data)-1):
    rects[i].remove()
    arrows[i].remove()
circ0=ax2.add_artist(plt.Circle((hier_data[2,0],hier_data[2,1]), 0.2, color='black'))
circ1=ax2.add_artist(plt.Circle((hier_data[11,0],hier_data[11,1]), 0.2, color='black'))
fig_caption(text="The similarity between points i and j is equivalent...", img_count=img_count)
rect0=ax1.add_patch(patches.Rectangle((11, len(hier_data)-1-2),1,1,fill=False,lw=3))
line0=ax2.arrow(hier_data[2,0], hier_data[2,1], hier_data[11,0]-hier_data[2,0], hier_data[11,1]-hier_data[2,1],
               head_width=0.5, head_length=0.5, fc='k', ec='k',length_includes_head=True)
fig_caption(text="The similarity between points i and j is equivalent...", img_count=img_count)
line1=ax2.arrow(hier_data[11,0], hier_data[11,1], hier_data[2,0]-hier_data[11,0], hier_data[2,1]-hier_data[11,1],
               head_width=0.5, head_length=0.5, fc='k', ec='k',length_includes_head=True)
rect1=ax1.add_patch(patches.Rectangle((2,len(hier_data)-1-11),1,1,fill=False,lw=3))
fig_caption(text="to the similarity between j and i...", img_count=img_count)
fig_caption(text="i.e. the matrix is symmetric", img_count=img_count)
rect0.remove()
rect1.remove()
line0.remove()
line1.remove()
circ0.remove()
circ1.remove()
for i in range(len(hier_data)):
    ax2.add_artist(plt.Circle((hier_data[i,0],hier_data[i,1]), 0.2, color='black'))
fig_caption(text="Typically, the similarity between a point and itself is zero...", img_count=img_count)
for i in range(len(hier_data)):
    rects.append(ax1.add_patch(patches.Rectangle((i, len(hier_data)-1-i),1,1,fill=False,lw=1.5)))
fig_caption(text="i.e. the diagonal of a similarity matrix is zero", img_count=img_count)
fig_caption(text="i.e. the diagonal of a similarity matrix is zero", img_count=img_count)
fig_caption(text="In affinity propogation, the diagonal of S represents the input preference...", img_count=img_count)
fig_caption(text="In affinity propogation, the diagonal of S represents the input preference...", img_count=img_count)
fig_caption(text="which determines the likelihood of a point becomming an exemplar (cluster centre)", img_count=img_count)
fig_caption(text="which determines the likelihood of a point becomming an exemplar (cluster centre)", img_count=img_count)
#sim_matrix.flat[::(13 + 1)]
sim_matrix.flat[::(len(hier_data) + 1)] = np.median(sim_matrix)
fig_caption(text="A common value is the median of all similarity values", img_count=img_count)
sns.heatmap(sim_matrix, vmax=0,cmap="YlGnBu_r",
            square=True, xticklabels=False, yticklabels=False,
            linewidths=.5, cbar=False, cbar_ax=None, ax=ax1)
fig_caption(text="A common value is the median of all similarity values", img_count=img_count)
fig_caption(text="More negative values will likely produce fewer exemplars (i.e. clusters)", img_count=img_count)
sim_matrix.flat[::(len(hier_data) + 1)] = -100
sns.heatmap(sim_matrix, vmax=0,cmap="YlGnBu_r",
            square=True, xticklabels=False, yticklabels=False,
            linewidths=.5, cbar=False, cbar_ax=None, ax=ax1)
fig_caption(text="More negative values will likely produce fewer exemplars (i.e. clusters)", img_count=img_count)
fig_caption(text="More positive values will likely return more exemplars (i.e. clusters)", img_count=img_count)
sim_matrix.flat[::(len(hier_data) + 1)] = -10
sns.heatmap(sim_matrix, vmax=0,cmap="YlGnBu_r",
            square=True, xticklabels=False, yticklabels=False,
            linewidths=.5, cbar=False, cbar_ax=None, ax=ax1)
fig_caption(text="More positive values will likely return more exemplars (i.e. clusters)", img_count=img_count)
plt.show()